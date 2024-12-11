{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}

{-| 
This module re-exports the AST from "Language.Python.Common.AST" and adds some
helpful utility functions and types.
-}
module Panini.Frontend.Python.AST
  ( module Language.Python.Common.AST
  , pattern IsVar
  , pattern IsString
  , expectVars
  , assignOpToOp
  , pattern ArgExprs
  , VarMention(..)
  , stmtVars
  , exprVars
  , exprVarsN
  , paramNames
  , paramDefaults
  , hasDefaultValue
  , sliceExprs
  , raiseExprs
  , mapRaiseExprsM
  , yieldArgExpr
  , dictKeyDatumListExprs
  ) where

import Data.Maybe
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Language.Python.Common.AST
import Panini.Frontend.Python.Strings 
import Prelude

------------------------------------------------------------------------------

pattern IsVar :: String -> Expr annot
pattern IsVar name <- Var { var_ident = Ident { ident_string = name }}

expectVars :: [Expr a] -> Maybe [Ident a]
expectVars = go []
 where
  go xs []            = Just (reverse xs)
  go xs (Var x _: es) = go (x:xs) es
  go _  _             = Nothing

------------------------------------------------------------------------------

pattern IsString :: String -> Expr annot
pattern IsString str <- Strings { strings_strings = [decodeStringLiteral -> Just str] }

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

pattern ArgExprs :: [Expr a] -> [Argument a]
pattern ArgExprs xs <- (expectArgExprs -> Just xs)

expectArgExprs :: [Argument a] -> Maybe [Expr a]
expectArgExprs = go []
 where
  go es []                 = Just (reverse es)
  go es (ArgExpr e _ : xs) = go (e:es) xs
  go _  _                  = Nothing

------------------------------------------------------------------------------

data VarMention v = Assigned v | Used v
  deriving stock (Eq, Ord, Show)

stmtVars :: Ord v => (Ident a -> v) -> Statement a -> Set (VarMention v)
stmtVars f = \case
  Import          {}   -> []
  FromImport      {}   -> []
  While           {..} -> used while_cond
  For             {..} -> assdN for_targets <> used for_generator
  AsyncFor        {..} -> stmtVars f for_stmt
  Fun             {..} -> usedN (paramDefaults fun_args)
  AsyncFun        {..} -> stmtVars f fun_def
  Class           {}   -> []
  Conditional     {..} -> usedN (map fst cond_guards)
  Assign          {..} -> assdN assign_to <> used assign_expr
  AugmentedAssign {..} -> assd aug_assign_to <> usedN [aug_assign_to, aug_assign_expr]
  AnnotatedAssign {..} -> assd ann_assign_to <> (usedM ann_assign_expr)
  Decorated       {}   -> undefined  -- TODO
  Return          {..} -> usedM return_expr
  Try             {}   -> []
  Raise           {..} -> usedN (raiseExprs raise_expr)
  With            {..} -> Set.unions [used e <> assdM t | (e,t) <- with_context]
  AsyncWith       {..} -> stmtVars f with_stmt
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
  assd  = Set.map Assigned . exprVars f
  assdN = Set.map Assigned . exprVarsN f
  assdM = maybe [] assd
  used  = Set.map Used . exprVars f
  usedN = Set.map Used . exprVarsN f
  usedM = maybe [] used

-- | All /free/ variables in the given expression.
exprVars :: Ord v => (Ident a -> v) -> Expr a -> Set v
exprVars f = \case
  Var        {..} -> [f var_ident]
  Call       {..} -> exprVarsN f (call_fun : map arg_expr call_args)
  Subscript  {..} -> exprVarsN f [subscriptee, subscript_expr]
  SlicedExpr {..} -> exprVarsN f (slicee : concatMap sliceExprs slices)
  CondExpr   {..} -> exprVarsN f [ce_true_branch, ce_condition, ce_false_branch]
  BinaryOp   {..} -> exprVarsN f [left_op_arg, right_op_arg]
  UnaryOp    {..} -> exprVars f op_arg
  Dot        {..} -> exprVars f dot_expr
  Lambda     {..} -> (exprVars f lambda_body \\ paramNames f lambda_args)
                      <> exprVarsN f (paramDefaults lambda_args)
  Tuple      {..} -> exprVarsN f tuple_exprs
  Yield      {..} -> maybe [] (exprVars f . yieldArgExpr) yield_arg  
  Await      {..} -> exprVars f await_expr
  Generator  {}   -> undefined  -- TODO
  ListComp   {}   -> undefined  -- TODO
  List       {..} -> exprVarsN f list_exprs
  Dictionary {..} -> exprVarsN f (dictKeyDatumListExprs dict_mappings)
  DictComp   {}   -> undefined  -- TODO
  Set        {..} -> exprVarsN f set_exprs
  SetComp    {}   -> undefined  -- TODO
  Starred    {..} -> exprVars f starred_expr
  Paren      {..} -> exprVars f paren_expr
  StringConversion {..} -> exprVars f backquoted_expr
  _               -> []

exprVarsN :: Ord v => (Ident a -> v) -> [Expr a] -> Set v
exprVarsN f = Set.unions . map (exprVars f)

paramNames :: Ord v => (Ident a -> v) -> [Parameter a] -> Set v
paramNames f = Set.fromList . concatMap go
 where
  go    Param          {..} = [f param_name]
  go    VarArgsPos     {..} = [f param_name]
  go    VarArgsKeyword {..} = [f param_name]
  go    EndPositional  {}   = []
  go    UnPackTuple    {..} = goTup param_unpack_tuple  
  goTup ParamTupleName {..} = [f param_tuple_name]
  goTup ParamTuple     {..} = concatMap goTup param_tuple

paramDefaults :: [Parameter a] -> [Expr a]
paramDefaults = catMaybes . concatMap go
 where
  go Param       {..} = [param_default]
  go UnPackTuple {..} = [param_default]
  go _                = []

hasDefaultValue :: Parameter a -> Bool
hasDefaultValue = \case
  Param       { param_default = Just _ } -> True
  UnPackTuple { param_default = Just _ } -> True
  _                                      -> False

sliceExprs :: Slice a -> [Expr a]
sliceExprs = \case
  SliceProper e1 e2 e3 _ -> catMaybes $ [e1,e2] ++ catMaybes [e3]
  SliceExpr e _          -> [e]
  SliceEllipsis _        -> []

raiseExprs :: RaiseExpr a -> [Expr a]
raiseExprs = \case
  RaiseV3 es -> unwrap2 es
  RaiseV2 es -> unwrap3 es

mapRaiseExprsM 
  :: Monad m => (Expr a -> m (Expr b)) -> RaiseExpr a -> m (RaiseExpr b)
mapRaiseExprsM f = \case
  RaiseV3 es -> RaiseV3 <$> wrap2 <$> mapM f (unwrap2 es)
  RaiseV2 es -> RaiseV2 <$> wrap3 <$> mapM f (unwrap3 es)

yieldArgExpr :: YieldArg a -> Expr a
yieldArgExpr = \case
  YieldFrom e _ -> e  
  YieldExpr e   -> e

dictKeyDatumListExprs :: [DictKeyDatumList a] -> [Expr a]
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

wrap2 :: [a] -> Maybe (a, Maybe a)
wrap2 = \case  
  a:b:_ -> Just (a, Just b)
  a:_   -> Just (a, Nothing)
  []    -> Nothing

wrap3 :: [a] -> Maybe (a, Maybe (a, Maybe a))
wrap3 = \case  
  a:b:c:_ -> Just (a, Just (b, Just c))
  a:b:_   -> Just (a, Just (b, Nothing))
  a:_     -> Just (a, Nothing)
  []      -> Nothing
