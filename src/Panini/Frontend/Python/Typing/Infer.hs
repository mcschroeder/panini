{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
module Panini.Frontend.Python.Typing.Infer (Typed, infer) where

import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Bifunctor
import Data.Generics.Uniplate.DataOnly
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Panini.Frontend.Python.AST as Py
import Panini.Frontend.Python.Typing.Builtins
import Panini.Frontend.Python.Typing.Monad
import Panini.Frontend.Python.Typing.Pretty ()
import Panini.Frontend.Python.Typing.PyType (PyType)
import Panini.Frontend.Python.Typing.PyType qualified as PyType
import Panini.Frontend.Python.Typing.TypeInfo
import Panini.Frontend.Python.Typing.Unify
import Panini.Panic
import Prelude

--import Debug.Trace
--import Panini.Pretty

------------------------------------------------------------------------------

infer :: Module a -> Either TypeError (Typed Module a)
infer (Module stmts) = runInfer $ do
  stmtsTypedMeta <- mapM inferStmt stmts
  --traceM $ showPretty stmtsTypedMeta
  constraints    <- lift $ gets subConstraints
  metaVarContext <- unify constraints
  let stmtsTyped  = map (resolveMetaVars metaVarContext) stmtsTypedMeta
  return          $ Module stmtsTyped

resolveMetaVars :: Functor f => IntMap PyType -> Typed f a -> Typed f a
resolveMetaVars ctx = fmap (first (fmap resolve))
 where
  resolve = rewrite $ \case
    PyType.MetaVar i -> IntMap.lookup i ctx
    _                -> Nothing

------------------------------------------------------------------------------

untyped :: Functor t => t a -> Typed t a
untyped = fmap (Nothing,)

-- | Note: this sets the type for the given expression and all sub-expressions!
setType :: Functor t => t a -> PyType -> Typed t a
setType e t = fmap (Just t,) e

------------------------------------------------------------------------------

inferStmt :: Statement a -> Infer (Typed Statement a)
inferStmt = \case
  s@Import{} -> pure (untyped s)
  s@FromImport{} -> pure (untyped s)

  -- TODO: bool heuristic?
  While{..} -> While <$> inferExpr while_cond 
                     <*> mapM inferStmt while_body 
                     <*> mapM inferStmt while_else 
                     <*> pure (Nothing, stmt_annot)

  For {..} -> do
    generator <- inferExpr for_generator
    targets <- mapM inferExpr for_targets
    let elemType = case map typeOf targets of
                      []                     -> impossible
                      [t]                    -> t
                      (t:ts) | all (== t) ts -> PyType.Iterable t
                             | otherwise     -> PyType.Iterable PyType.Any
    constrain $ typeOf generator :≤ PyType.Iterable elemType
    forM_ targets $ \target -> case target of
      IsVar x -> registerVar x (typeOf target)
      _       -> pure ()
    body <- mapM inferStmt for_body
    else_ <- mapM inferStmt for_else
    -- TODO: unregister target vars
    return For
      { for_targets = targets
      , for_generator = generator
      , for_body = body
      , for_else = else_
      , stmt_annot = (Nothing, stmt_annot)
      }

  AsyncFor{..} -> AsyncFor <$> inferStmt for_stmt 
                           <*> pure (Nothing, stmt_annot)

  -- TODO: support generics
  -- TODO: limit scope of variables
  Fun{..} -> do
    parameters <- mapM inferParam fun_args
    resultHint <- mapM typifyHint fun_result_annotation
    returnType <- maybe newMetaVar (pure . typeOf) resultHint
    let funType = mkFunType parameters returnType
    registerVar fun_name.ident_string funType
    pushEmptyReturnTypeStackFrame
    body <- mapM inferStmt fun_body
    popReturnTypeStackFrame >>= \case
      [] -> constrain $ PyType.None :≤ returnType
      ts -> mapM_ constrain $ map (:≤ returnType) ts
    Fun <$> pure (setType fun_name funType)
        <*> pure parameters
        <*> pure resultHint
        <*> pure body
        <*> pure (Nothing, stmt_annot)

  AsyncFun{..} -> AsyncFun <$> inferStmt fun_def 
                           <*> pure (Nothing, stmt_annot)

  -- TODO: properly support classes
  Class{..} -> Class <$> pure (untyped class_name) 
                     <*> pure (map untyped class_args)
                     <*> mapM inferStmt class_body 
                     <*> pure (Nothing, stmt_annot)

  -- TODO: bool heuristic?
  Conditional{..} -> Conditional <$> mapM inferGuard cond_guards
                                 <*> mapM inferStmt cond_else
                                 <*> pure (Nothing, stmt_annot)
   where
    inferGuard (cond,body) = (,) <$> inferExpr cond 
                                 <*> mapM inferStmt body

  Assign{..} -> do
    from <- inferExpr assign_expr
    targets <- mapM inferExpr assign_to
    let targetType = case map typeOf targets of
                      []                     -> impossible
                      [t]                    -> t
                      (t:ts) | all (== t) ts -> PyType.Iterable t
                             | otherwise     -> PyType.Iterable PyType.Any
    constrain $ typeOf from :≤ targetType
    forM_ targets $ \target -> case target of
      IsVar x -> registerVar x (typeOf target)
      _       -> pure ()
    return Assign
      { assign_to   = targets
      , assign_expr = from
      , stmt_annot  = (Nothing, stmt_annot)
      }

  AugmentedAssign{..} -> do
    lhs <- inferExpr aug_assign_to
    rhs <- inferExpr aug_assign_expr
    let t = PyType.Callable [typeOf rhs, typeOf rhs] (typeOf lhs)
    funType <- typeOfBinaryOp $ assignOpToOp aug_assign_op
    constrain $ funType :*≤ t
    return AugmentedAssign 
      { aug_assign_to   = lhs
      , aug_assign_op   = fmap (Just t,) aug_assign_op
      , aug_assign_expr = rhs
      , stmt_annot      = (Nothing, stmt_annot)
      }

  AnnotatedAssign{..} -> do
    hint <- typifyHint ann_assign_annotation
    target <- inferExpr ann_assign_to
    constrain $ typeOf hint :≤ typeOf target
    from <- mapM inferExpr ann_assign_expr
    whenJust from $ \e -> do
      constrain $ typeOf e :≤ typeOf target
      case e of
        IsVar x -> registerVar x (typeOf e)
        _       -> pure ()
    return AnnotatedAssign
      { ann_assign_annotation = hint
      , ann_assign_to         = target
      , ann_assign_expr       = from
      , stmt_annot            = (Nothing, stmt_annot)
      }

  -- TODO: properly support decorators
  Decorated{..} -> Decorated <$> pure (map untyped decorated_decorators) 
                             <*> inferStmt decorated_def 
                             <*> pure (Nothing, stmt_annot)
  
  Return { return_expr = Just e, ..} -> do
    e' <- inferExpr e
    addReturnTypeToStackFrame (typeOf e')
    return $ Return (Just e') (Nothing, stmt_annot)

  Return { return_expr = Nothing, ..} -> do
    addReturnTypeToStackFrame PyType.None
    return $ Return Nothing (Nothing, stmt_annot)
  
  Try{..} -> Try <$> mapM inferStmt try_body
                 <*> mapM inferHandler try_excepts
                 <*> mapM inferStmt try_else  
                 <*> mapM inferStmt try_finally
                 <*> pure (Nothing, stmt_annot)
   where
    -- TODO: bind exception targets in handler clause
    inferHandler Handler{..} = Handler <$> pure (untyped handler_clause)
                                       <*> mapM inferStmt handler_suite 
                                       <*> pure (Nothing, handler_annot)

  Raise{..} -> Raise <$> mapRaiseExprsM inferExpr raise_expr 
                     <*> pure (Nothing, stmt_annot)
  
  -- TODO: properly support with expressions: assign target vars in body  
  With{..} -> With <$> mapM inferContext with_context 
                   <*> mapM inferStmt with_body 
                   <*> pure (Nothing, stmt_annot)
   where
    inferContext (expr,target) = (,) <$> inferExpr expr
                                     <*> mapM inferExpr target

  AsyncWith{..} -> AsyncWith <$> inferStmt with_stmt
                             <*> pure (Nothing, stmt_annot)

  Pass{..} -> Pass <$> pure (Nothing, stmt_annot)
  Break{..} -> Break <$> pure (Nothing, stmt_annot)
  Continue{..} -> Continue <$> pure (Nothing, stmt_annot)

  -- TODO: remove variables from scope
  Delete{..} -> Delete <$> mapM inferExpr del_exprs 
                       <*> pure (Nothing, stmt_annot)

  StmtExpr{..} -> StmtExpr <$> inferExpr stmt_expr 
                           <*> pure (Nothing, stmt_annot)
  
  -- TODO: track variable scopes
  s@Global{} -> pure (untyped s)
  s@NonLocal{} -> pure (untyped s)

  -- TODO: bool heuristic? (for first argument)
  Assert{..} -> Assert <$> mapM inferExpr assert_exprs
                       <*> pure (Nothing, stmt_annot)

  Print{..} -> Print <$> pure print_chevron
                     <*> mapM inferExpr print_exprs
                     <*> pure print_trailing_comma
                     <*> pure (Nothing, stmt_annot)

  s@Exec{} -> pure (untyped s)


mkFunType :: [Typed Parameter a] -> PyType -> PyType
mkFunType params returnType = PyType.Union 
  [ PyType.Callable (fixedTypes ++ possibleTypes) returnType
  | let (fixedParams, optionalParams) = break hasDefaultValue params
  , let fixedTypes = map typeOf fixedParams
  , possibleTypes <- List.subsequences $ map typeOf optionalParams
  ]

-- TODO: also replace 'Any' w/ fresh metavars?
typeOfBuiltinFunction :: String -> Infer [PyType]
typeOfBuiltinFunction f = case Map.lookup f builtinFunctions of
  Nothing -> List.singleton <$> newMetaVar
  Just t0 -> evalStateT (mapM go t0) mempty
 where
  go = transformM $ \case
    PyType.TypeVar x -> Map.lookup x <$> get >>= \case
                          Just t -> return t
                          Nothing -> do
                            t <- lift newMetaVar
                            modify' $ Map.insert x t
                            return t
    t -> return t

inferExpr :: Expr a -> Infer (Typed Expr a)
inferExpr = \case
  Var{..} -> do
    let x = var_ident.ident_string
    t <- typeOfVar x >>= \case
      Just t -> pure t
      Nothing -> do
        t <- newMetaVar
        registerVar x t
        pure t
    return Var 
      { var_ident = fmap (Just t,) var_ident
      , expr_annot = (Just t, expr_annot)
      }
  
  e@Int            {} -> pure $ setType e PyType.Int
  e@LongInt        {} -> pure $ setType e PyType.Int
  e@Float          {} -> pure $ setType e PyType.Float
  e@Imaginary      {} -> pure $ setType e PyType.Complex
  e@Bool           {} -> pure $ setType e PyType.Bool
  e@None           {} -> pure $ setType e PyType.None
  e@Ellipsis       {} -> pure . setType e =<< newMetaVar
  e@ByteStrings    {} -> pure $ setType e PyType.Bytes
  e@Strings        {} -> pure $ setType e PyType.Str
  e@UnicodeStrings {} -> pure $ setType e PyType.Str

  Call { call_fun = dotExpr@Dot{}, .. } -> do
    funTy1 <- typeOfBuiltinFunction dotExpr.dot_attribute.ident_string
    objExpr <- inferExpr dotExpr.dot_expr
    funArgs <- mapM inferArg call_args
    let argTys = typeOf objExpr : map typeOf funArgs
    μ <- newMetaVar
    let funTy2 = PyType.Callable argTys μ
    constrain $ funTy1 :*≤ funTy2
    return Call
      { call_fun = Dot 
        { dot_expr = objExpr
        , dot_attribute = fmap (Just funTy2,) dotExpr.dot_attribute
        , expr_annot = (Just funTy2, dotExpr.expr_annot)
        }
      , call_args = funArgs
      , expr_annot = (Just μ, expr_annot)
      }

  Call { call_fun = funExpr@(IsVar x), .. } -> do
    funTy1 <- typeOfBuiltinFunction x
    funArgs <- mapM inferArg call_args
    μ <- newMetaVar
    let funTy2 = PyType.Callable (map typeOf funArgs) μ
    constrain $ funTy1 :*≤ funTy2
    return Call
      { call_fun = untyped funExpr
      , call_args = funArgs
      , expr_annot = (Just μ, expr_annot) 
      }

  Call{..} -> do    
    funArgs <- mapM inferArg call_args
    μ <- newMetaVar
    let funType = PyType.Callable (map typeOf funArgs) μ
    funExpr <- inferExpr call_fun
    constrain $ typeOf funExpr :≤ funType
    return Call 
      { call_fun   = funExpr
      , call_args  = funArgs
      , expr_annot = (Just μ, expr_annot)
      }

  Subscript{..} -> do
    e1 <- inferExpr subscriptee
    e2 <- inferExpr subscript_expr 
    μ <- newMetaVar
    let t1 = PyType.Callable [typeOf e1, typeOf e2] μ
    t2 <- typeOfBuiltinFunction "__getitem__"
    constrain $ t2 :*≤ t1
    return Subscript { subscriptee    = e1
                     , subscript_expr = e2
                     , expr_annot     = (Just μ, expr_annot)
                     }
  
  -- TODO: more sophisticated analysis of slice types?
  SlicedExpr{..} -> do
    e1 <- inferExpr slicee
    e2 <- mapM inferSlice slices
    let sliceTy | [x] <- e2 = typeOf x
                | otherwise = PyType.Tuple $ map typeOf e2
    μ <- newMetaVar
    let t1 = PyType.Callable [typeOf e1, sliceTy] μ
    t2 <- typeOfBuiltinFunction "__getitem__"
    constrain $ t2 :*≤ t1
    return SlicedExpr 
      { slicee     = e1
      , slices     = e2
      , expr_annot = (Just μ, expr_annot)
      }

  -- TODO: bool heuristic for condition?
  CondExpr{..} -> do
    trueExpr <- inferExpr ce_true_branch
    condExpr <- inferExpr ce_condition    
    falsExpr <- inferExpr ce_false_branch
    μ <- newMetaVar
    constrain $ typeOf trueExpr :≤ μ
    constrain $ typeOf falsExpr :≤ μ
    return CondExpr 
      { ce_true_branch  = trueExpr
      , ce_condition    = condExpr
      , ce_false_branch = falsExpr
      , expr_annot      = (Just μ, expr_annot)
      }

  BinaryOp{..} -> do
    leExpr <- inferExpr left_op_arg    
    riExpr <- inferExpr right_op_arg
    μ <- newMetaVar
    let t1 = PyType.Callable [typeOf leExpr, typeOf riExpr] μ
    t2 <- typeOfBinaryOp operator
    constrain $ t2 :*≤ t1
    return BinaryOp 
      { operator     = setType operator t1
      , left_op_arg  = leExpr
      , right_op_arg = riExpr
      , expr_annot   = (Just μ, expr_annot)
      }

  UnaryOp{..} -> do
    expr <- inferExpr op_arg    
    μ <- newMetaVar
    let t1 = PyType.Callable [typeOf expr] μ
    t2 <- typeOfUnaryOp operator
    constrain $ t2 :*≤ t1
    return UnaryOp 
      { operator   = setType operator t1
      , op_arg     = expr
      , expr_annot = (Just μ, expr_annot)
      }
  
  -- TODO: infer type based on attributes of known object types
  -- Dot{..} -> do
  --   obj <- inferExpr dot_expr
  --   t <- typeOfBuiltinFunction dot_attribute.ident_string
  --   return $ Dot 
  --     { dot_expr      = obj
  --     , dot_attribute = setType dot_attribute t
  --     , expr_annot    = (Just t, expr_annot)
  --     }
  
  Lambda{..} -> do
    params <- mapM inferParam lambda_args
    body   <- inferExpr lambda_body
    let t   = PyType.Callable (map typeOf params) (typeOf body)
    return  $ Lambda 
      { lambda_args = params
      , lambda_body = body
      , expr_annot  = (Just t, expr_annot) 
      }
  
  Tuple{..} -> do
    exprs <- mapM inferExpr tuple_exprs
    let t  = PyType.Tuple (map typeOf exprs)
    return $ Tuple
      { tuple_exprs = exprs
      , expr_annot  = (Just t, expr_annot) 
      }

  -- TODO: yields
  -- Yield{..} -> undefined
  
  Await{..} -> do
    e <- inferExpr await_expr
    return Await 
      { await_expr = e
      , expr_annot = (Just (typeOf e), expr_annot)
      }

  List{..} -> do
    items <- mapM inferExpr list_exprs
    μ <- newMetaVar
    mapM_ (constrain . (:≤ μ)) (map typeOf items)
    return List
      { list_exprs = items
      , expr_annot = (Just (PyType.List μ), expr_annot)
      }
  
  Dictionary{..} -> do
    keyType <- newMetaVar
    valueType <- newMetaVar
    constrain $ keyType :≤ PyType.Hashable
    forM_ dict_mappings $ \case
      DictUnpacking _ -> pure () -- TODO
      DictMappingPair k v -> do
        kt <- typeOf <$> inferExpr k
        vt <- typeOf <$> inferExpr v
        constrain $ kt :≤ keyType
        constrain $ vt :≤ valueType
    return Dictionary
      { dict_mappings = map untyped dict_mappings
      , expr_annot    = (Just (PyType.Dict keyType valueType), expr_annot)
      }    
    
  Py.Set{..} -> do
    items <- mapM inferExpr set_exprs
    μ <- newMetaVar
    mapM_ (constrain . (:≤ μ)) (map typeOf items)
    return Py.Set
      { set_exprs = items
      , expr_annot = (Just (PyType.Set μ), expr_annot)
      }
  
  Starred{..} -> do
    e <- inferExpr starred_expr
    let t = typeOf e
    constrain $ t :≤ PyType.Iterable PyType.Any
    Starred 
      <$> pure e
      <*> pure (Just t, expr_annot)
  
  Paren{..} -> do
    e <- inferExpr paren_expr
    Paren 
      <$> pure e 
      <*> pure (Just (typeOf e), expr_annot)
  
  StringConversion{..} -> 
    StringConversion
      <$> inferExpr backquoted_expr
      <*> pure (Just PyType.Str, expr_anot)

  -- TODO: comprehensions  
  -- Generator{..} -> undefined
  -- ListComp{..} -> undefined
  -- DictComp{..} -> undefined
  -- SetComp{..} -> undefined

  e -> return $ fmap (Just PyType.Any,) e

-- | Note: this will add the types of named parameters to the variable context!
inferParam :: Parameter a -> Infer (Typed Parameter a)
inferParam = \case
  Param {..} -> do
    typeHintExpr <- mapM typifyHint param_py_annotation
    let hintedType = fmap typeOf typeHintExpr
    defaultExpr <- mapM inferExpr param_default
    let defaultType = fmap typeOf defaultExpr
    paramType <- newMetaVar
    mapM_ (constrain . (:≤ paramType)) hintedType
    mapM_ (constrain . (:≤ paramType)) defaultType
    let x = param_name.ident_string
    registerVar x paramType
    return Param
      { param_name          = fmap (Just paramType,) param_name
      , param_py_annotation = typeHintExpr
      , param_default       = defaultExpr
      , param_annot         = (Just paramType, param_annot)
      }
  
  p -> throwE $ UnsupportedParam p

inferArg :: Argument a -> Infer (Typed Argument a)
inferArg = \case
  ArgExpr{..} -> do
    expr <- inferExpr arg_expr
    return ArgExpr
      { arg_expr = expr
      , arg_annot = (Just $ typeOf expr, arg_annot)
      }

  a -> throwE $ UnsupportedArg a

inferSlice :: Slice a -> Infer (Typed Slice a)
inferSlice = \case
  SliceProper{..} -> SliceProper <$> mapM inferExpr slice_lower 
                                 <*> mapM inferExpr slice_upper
                                 <*> mapM (mapM inferExpr) slice_stride
                                 <*> pure (Just PyType.Slice, slice_annot)
  
  SliceExpr{..} -> do
    e <- inferExpr slice_expr
    SliceExpr <$> pure e <*> pure (Just (typeOf e), slice_annot)
  
  SliceEllipsis{..} -> 
    SliceEllipsis <$> pure (Just PyType.Ellipsis, slice_annot)

typeOfUnaryOp :: Op a -> Infer [PyType]
typeOfUnaryOp = \case
  Plus   {} -> typeOfBuiltinFunction "__pos__"
  Minus  {} -> typeOfBuiltinFunction "__neg__"
  Invert {} -> typeOfBuiltinFunction "__invert__"
  Not    {} -> pure $ [PyType.Callable [PyType.Object] PyType.Bool]
  _         -> impossible  -- all other operators are binary only

typeOfBinaryOp :: Op a -> Infer [PyType]
typeOfBinaryOp = \case
  And               {} -> pure [PyType.Callable [PyType.Object, PyType.Object] PyType.Bool]
  Or                {} -> pure [PyType.Callable [PyType.Object, PyType.Object] PyType.Bool]
  Not               {} -> impossible  -- unary only
  Exponent          {} -> typeOfBuiltinFunction "__pow__"
  LessThan          {} -> typeOfBuiltinFunction "__lt__"
  GreaterThan       {} -> typeOfBuiltinFunction "__gt__"
  Equality          {} -> typeOfBuiltinFunction "__eq__"
  GreaterThanEquals {} -> typeOfBuiltinFunction "__ge__"
  LessThanEquals    {} -> typeOfBuiltinFunction "__le__"
  NotEquals         {} -> typeOfBuiltinFunction "__ne__"
  NotEqualsV2       {} -> typeOfBuiltinFunction "__ne__"
  Is                {} -> pure [PyType.Callable [PyType.Object, PyType.Object] PyType.Bool]
  IsNot             {} -> pure [PyType.Callable [PyType.Object, PyType.Object] PyType.Bool]
  In                {} -> map flipArgs <$> typeOfBuiltinFunction "__contains__"
  NotIn             {} -> map flipArgs <$> typeOfBuiltinFunction "__contains__"
  BinaryOr          {} -> typeOfBuiltinFunction "__or__"
  Xor               {} -> typeOfBuiltinFunction "__xor__"
  BinaryAnd         {} -> typeOfBuiltinFunction "__and__"
  ShiftLeft         {} -> typeOfBuiltinFunction "__lshift__"
  ShiftRight        {} -> typeOfBuiltinFunction "__rshift__"
  Multiply          {} -> typeOfBuiltinFunction "__mul__"
  Plus              {} -> typeOfBuiltinFunction "__add__"
  Minus             {} -> typeOfBuiltinFunction "__sub__"
  Divide            {} -> typeOfBuiltinFunction "__truediv__"
  FloorDivide       {} -> typeOfBuiltinFunction "__floordiv__"
  MatrixMult        {} -> typeOfBuiltinFunction "__matmul__"
  Invert            {} -> impossible  -- unary only
  Modulo            {} -> typeOfBuiltinFunction "__mod__"
 where
  flipArgs = \case
    PyType.Callable [a,b] c -> PyType.Callable [b,a] c
    PyType.Union xs         -> PyType.Union (map flipArgs xs)
    _                       -> impossible

typifyHint :: Expr a -> Infer (Typed Expr a)
typifyHint e = case e of
  IsVar "bool" -> return $ setType e PyType.Bool
  IsVar "int"  -> return $ setType e PyType.Int
  IsVar "str"  -> return $ setType e PyType.Str
  _ -> throwE $ UnsupportedTypeHint e
