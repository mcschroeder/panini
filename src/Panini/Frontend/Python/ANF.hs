{-# LANGUAGE RecordWildCards #-}

module Panini.Frontend.Python.ANF where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Panini.Frontend.Python.AST as Py
import Panini.Frontend.Python.CFG
import Panini.Frontend.Python.SSA
import Panini.Frontend.Common.SSA (VarSet)
import Panini.Frontend.Python.DomTree
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Panini.Syntax
import Panini.Pretty
import Panini.Provenance
import Prelude
import Data.Text qualified as Text

type Transpiler a = StateT () (Except String) a

transpile :: CFG -> Either String Program
transpile cfg = runExcept (evalStateT (transpileTopLevel cfg) ())

transpileTopLevel :: CFG -> Transpiler Program
transpileTopLevel cfg = go cfg.entry
 where
  go l = case cfg.nodeMap IntMap.! l of
    FunDef{..} -> do
      k <- transpileFun _body
      lam <- mkLams _args k
      let def = Define (mangle _name) lam
      (def :) <$> go _next

    Block{..} -> do
      imports <- concat <$> mapM goImport _stmts
      (imports ++) <$> go _next
    
    Branch{} -> lift $ throwE $ "UnsupportedTopLevel: branch" -- TODO
    BranchFor{} -> lift $ throwE $ "UnsupportedTopLevel: for" -- TODO

    Exit -> return []  

  goImport = \case
    Py.Import{} -> return [] -- TODO: transpile imports and add to builder environment
    stmt        -> lift $ throwE $ "UnsupportedTopLevel: " ++ showPretty stmt -- TODO
  
mkLams :: [ParameterSpan] -> Term -> Transpiler Term 
mkLams ps k0 = foldM mkLam k0 (reverse ps)
 where  
  mkLam k p = case p of
    Param
      { param_name
      , param_py_annotation = Just typeHint
      , param_default = Nothing
      } 
      -> case typeHintToType typeHint of
        Just t -> return $ Lam (mangle param_name) t k (pySpanToPV p.param_annot)
        Nothing -> lift $ throwE $ "Unsupported Python type: " ++ show typeHint

    Param 
      { param_default = Just _ 
      } 
      -> lift $ throwE $ "Default parameter values are unsupported"

    Param 
      { param_py_annotation = Nothing 
      } 
      -> lift $ throwE $ "Missing type annotation for function parameter: " ++ showPretty p    
  
    _ -> lift $ throwE $ "Unsupported function parameter type: " ++ showPretty p

transpileFun :: CFG -> Transpiler Term
transpileFun = undefined

mangle :: IdentSpan -> Name
mangle Ident{..} = Name (Text.pack ident_string) (pySpanToPV ident_annot)

typeHintToType :: ExprSpan -> Maybe Type
typeHintToType = \case
  Py.Var{..} -> 
    let baseType b = TBase dummyName b (Known PTrue) (pySpanToPV expr_annot)
    in case var_ident.ident_string of
      "bool" -> Just (baseType TBool)
      "int"  -> Just (baseType TInt)
      "str"  -> Just (baseType TString)
      _      -> Nothing
  _ -> Nothing 


-- transpilreStmt stmt = case stmt of
--   Import          {}   -> undefined -- TODO: add import to environment
--   Assign          {..} -> assdN assign_to <> used assign_expr
--   AugmentedAssign {..} -> assd aug_assign_to <> used aug_assign_expr
--   AnnotatedAssign {..} -> assd ann_assign_to <> (usedM ann_assign_expr)
--   Return          {..} -> usedM return_expr
-- --Raise           {..} -> undefined  -- TODO
-- --With            {..} -> undefined  -- TODO
-- --Delete          {..} -> undefined  -- TODO
--   StmtExpr        {..} -> undefined  -- TODO
--   Assert          {..} -> map mkAssert assert_exprs
--   Print           {..} -> usedN print_exprs -- TODO
--   Exec            {..} -> usedN $ exec_expr : unwrap2 exec_globals_locals  -- TODO
--   stmt                 -> throwE (Unsupported stmt)



-- mangle :: IdentSpan -> Var 
-- newVar :: m Var
-- newVarBasedOn :: IdentSpan -> m Var


-- transpile Block{..} = foldr go (mkGotoApp _next) _stmts
--  where
--   go stmt k = case stmt of
--     Assign 
--       { assign_to = [Py.Var x]
--       , assign_expr = Py.Var y 
--       } 
--       -> Let (mangle x) (Val (Var (mangle y))) k
    
--     Assign 
--       { assign_to = [Py.Var x]
--       , assign_expr = Py.Int { int_value } 
--       } 
--       -> Let (mangle x) (Val (Con (I int_value NoPV))) k
    
--     AugmentedAssign 
--       { aug_assign_to = Py.Var x
--       , aug_assign_expr = Py.Var y
--       , aug_assign_op = PlusAssign
--       }
--       -> Let (mangle x) (App (App (Var "add") (Val (Var mangle x))) (Val (Var (mangle y)))) k


