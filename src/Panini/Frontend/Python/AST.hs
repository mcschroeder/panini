{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}

{-| 
This module re-exports the AST from "Language.Python.Common.AST" and adds some
helpful utility functions and types.
-}
module Panini.Frontend.Python.AST
  ( module Language.Python.Common.AST
  , assignOpToOp
  , pattern PhiAssign
  , pySpanToPV
  , Var
  , VarMention(..)
  , stmtVars
  , exprVars
  , exprVarsN
  , paramNames
  , paramDefaults
  , sliceExprs
  , raiseExprs
  , yieldArgExpr
  , dictKeyDatumListExprs
  ) where

import Data.Maybe
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Language.Python.Common.AST
import Language.Python.Common.SrcLocation
import Panini.Provenance
import Prelude

------------------------------------------------------------------------------

assignOpToOp :: AssignOp annot -> Op annot
assignOpToOp = \case
  PlusAssign       a -> Plus        a
  MinusAssign      a -> Minus       a
  MultAssign       a -> Multiply    a
  DivAssign        a -> Divide      a
  ModAssign        a -> Modulo      a
  PowAssign        a -> Exponent    a
  BinAndAssign     a -> BinaryAnd   a
  BinOrAssign      a -> BinaryOr    a
  BinXorAssign     a -> Xor         a
  LeftShiftAssign  a -> ShiftLeft   a
  RightShiftAssign a -> ShiftRight  a
  FloorDivAssign   a -> FloorDivide a
  MatrixMultAssign a -> MatrixMult  a

------------------------------------------------------------------------------

pattern PhiAssign :: String -> [String] -> StatementSpan
pattern PhiAssign x xs = Assign [VarName x] (PhiCall xs) SpanEmpty_

pattern PhiCall :: [String] -> ExprSpan
pattern PhiCall vs = Call PhiFunc (ArgVars vs) SpanEmpty_

pattern PhiFunc :: ExprSpan
pattern PhiFunc = Var (Ident "%phi" SpanEmpty_) SpanEmpty_

pattern VarName :: String -> ExprSpan
pattern VarName x = Var (Ident x SpanEmpty_) SpanEmpty_

pattern SpanEmpty_ :: SrcSpan
pattern SpanEmpty_ <- _ where 
  SpanEmpty_ = SpanEmpty

pattern ArgVars :: [String] -> [ArgumentSpan]
pattern ArgVars xs <- (expectArgVars -> Just xs) where
  ArgVars xs = map (\x -> ArgExpr (VarName x) SpanEmpty) xs

expectArgVars :: [ArgumentSpan] -> Maybe [String]
expectArgVars = go []
 where
  go vs []                           = Just (reverse vs)
  go vs (ArgExpr (VarName v) _ : xs) = go (v:vs) xs
  go _  _                            = Nothing

------------------------------------------------------------------------------

pySpanToPV :: SrcSpan -> PV
pySpanToPV = \case
  SpanEmpty -> NoPV
  sp -> FromSource SrcLoc{..} Nothing
   where 
    file  = sp.span_filename
    begin = (startRow sp, startCol sp)
    end   = (endRow   sp, endCol   sp + 1)

------------------------------------------------------------------------------

type Var = String

data VarMention = Assigned Var | Used Var
  deriving stock (Eq, Ord, Show)

stmtVars :: StatementSpan -> Set VarMention
stmtVars = \case
  Import          {}   -> []
  FromImport      {}   -> []
  While           {..} -> used while_cond
  For             {..} -> assdN for_targets <> used for_generator
  AsyncFor        {..} -> stmtVars for_stmt
  Fun             {..} -> usedN (paramDefaults fun_args)
  AsyncFun        {..} -> stmtVars fun_def
  Class           {}   -> []
  Conditional     {..} -> usedN (map fst cond_guards)
  Assign          {..} -> assdN assign_to <> used assign_expr
  AugmentedAssign {..} -> assd aug_assign_to <> used aug_assign_expr
  AnnotatedAssign {..} -> assd ann_assign_to <> (usedM ann_assign_expr)
  Decorated       {}   -> undefined  -- TODO
  Return          {..} -> usedM return_expr
  Try             {}   -> []
  Raise           {..} -> usedN (raiseExprs raise_expr)
  With            {..} -> Set.unions [used e <> assdM t | (e,t) <- with_context]
  AsyncWith       {..} -> stmtVars with_stmt
  Pass            {}   -> []
  Break           {}   -> []
  Continue        {}   -> []
  Delete          {..} -> usedN del_exprs
  StmtExpr        {..} -> used stmt_expr
  Global          {}   -> []
  NonLocal        {}   -> []
  Assert          {..} -> usedN assert_exprs
  Print           {..} -> usedN print_exprs
  Exec            {..} -> usedN $ exec_expr : unwrap2 exec_globals_locals
 where
  assd  = Set.map Assigned . exprVars
  assdN = Set.map Assigned . exprVarsN
  assdM = maybe [] assd
  used  = Set.map Used . exprVars
  usedN = Set.map Used . exprVarsN
  usedM = maybe [] used

-- | All /free/ variables in the given expression.
exprVars :: ExprSpan -> Set Var
exprVars = \case
  Var        {..} -> [var_ident.ident_string]
  Call       {..} -> exprVarsN (call_fun : map arg_expr call_args)
  Subscript  {..} -> exprVarsN [subscriptee, subscript_expr]
  SlicedExpr {..} -> exprVarsN (slicee : concatMap sliceExprs slices)
  CondExpr   {..} -> exprVarsN [ce_true_branch, ce_condition, ce_false_branch]
  BinaryOp   {..} -> exprVarsN [left_op_arg, right_op_arg]
  UnaryOp    {..} -> exprVars op_arg
  Dot        {..} -> exprVars dot_expr
  Lambda     {..} -> (exprVars lambda_body \\ paramNames lambda_args)
                      <> exprVarsN (paramDefaults lambda_args)
  Tuple      {..} -> exprVarsN tuple_exprs
  Yield      {..} -> maybe [] (exprVars . yieldArgExpr) yield_arg  
  Await      {..} -> exprVars await_expr
  Generator  {}   -> undefined  -- TODO
  ListComp   {}   -> undefined  -- TODO
  List       {..} -> exprVarsN list_exprs
  Dictionary {..} -> exprVarsN (dictKeyDatumListExprs dict_mappings)
  DictComp   {}   -> undefined  -- TODO
  Set        {..} -> exprVarsN set_exprs
  SetComp    {}   -> undefined  -- TODO
  Starred    {..} -> exprVars starred_expr
  Paren      {..} -> exprVars paren_expr
  StringConversion {..} -> exprVars backquoted_expr
  _               -> []

exprVarsN :: [ExprSpan] -> Set Var
exprVarsN = Set.unions . map exprVars

paramNames :: [ParameterSpan] -> Set Var
paramNames = Set.fromList . concatMap go
 where
  go    Param          {..} = [param_name.ident_string]
  go    VarArgsPos     {..} = [param_name.ident_string]
  go    VarArgsKeyword {..} = [param_name.ident_string]
  go    EndPositional  {}   = []
  go    UnPackTuple    {..} = goTup param_unpack_tuple  
  goTup ParamTupleName {..} = [param_tuple_name.ident_string]
  goTup ParamTuple     {..} = concatMap goTup param_tuple

paramDefaults :: [ParameterSpan] -> [ExprSpan]
paramDefaults = catMaybes . concatMap go
 where
  go Param       {..} = [param_default]
  go UnPackTuple {..} = [param_default]
  go _                = []

sliceExprs :: SliceSpan -> [ExprSpan]
sliceExprs = \case
  SliceProper e1 e2 e3 _ -> catMaybes $ [e1,e2] ++ catMaybes [e3]
  SliceExpr e _          -> [e]
  SliceEllipsis _        -> []

raiseExprs :: RaiseExprSpan -> [ExprSpan]
raiseExprs = \case
  RaiseV3 es -> unwrap2 es
  RaiseV2 es -> unwrap3 es

yieldArgExpr :: YieldArgSpan -> ExprSpan
yieldArgExpr = \case
  YieldFrom e _ -> e  
  YieldExpr e   -> e

dictKeyDatumListExprs :: [DictKeyDatumListSpan] -> [ExprSpan]
dictKeyDatumListExprs = concatMap go
 where
  go (DictMappingPair e1 e2) = [e1,e2]
  go (DictUnpacking e)       = [e]


unwrap2 :: Maybe (a, Maybe a) -> [a]
unwrap2 = \case
  Nothing           -> []
  Just (a, Nothing) -> [a]
  Just (a, Just b)  -> [a,b]

unwrap3 :: Maybe (a, Maybe (a, Maybe a)) -> [a]
unwrap3 = \case
  Nothing    -> []
  Just (a,b) -> a : unwrap2 b
