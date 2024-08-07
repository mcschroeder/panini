{-# LANGUAGE RecordWildCards #-}

module Panini.Frontend.Python.Transpiler (transpile) where

import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Foldable
import Data.IntMap.Strict ((!))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.String
import Data.Text qualified as Text
import Panini.Frontend.Python.AST hiding (Var)
import Panini.Frontend.Python.AST qualified as Py
import Panini.Frontend.Python.Axioms
import Panini.Frontend.Python.CFG hiding (Error)
import Panini.Frontend.Python.DomTree
import Panini.Frontend.Python.Error
import Panini.Frontend.Python.Strings
import Panini.Frontend.Python.Typing.PyType (PyType)
import Panini.Frontend.Python.Typing.PyType qualified as PyType
import Panini.Frontend.Python.Typing.TypeInfo
import Panini.Panic
import Panini.Pretty
import Panini.Provenance
import Panini.Syntax
import Prelude

------------------------------------------------------------------------------

-- TODO: language-python needs to be updated to support newer Python syntax
-- TODO: be explicit about the Python version that is supported (semantics!)

transpile :: HasProvenance a => Typed DomTree a -> Either Error Program
transpile dom = runExcept (evalStateT (transpileTopLevel dom) env0)
 where
  env0 = TranspilerEnv 
    { varSource   = 0
    , typeContext = mempty
    }

------------------------------------------------------------------------------

data TranspilerEnv = TranspilerEnv
  { varSource   :: Int
  , typeContext :: Map Name Base
  }
  deriving stock (Show)

type Transpiler a = StateT TranspilerEnv (Except Error) a

newVar :: Transpiler Name
newVar = do
  i <- gets varSource
  modify' $ \env -> env { varSource = i + 1 }
  return $ Name (Text.pack $ "v" ++ show i) NoPV

mangle :: HasProvenance annot => Ident annot -> Name
mangle x = Name (Text.pack x.ident_string) (getPV x)

-- TODO: uniqueness
-- TODO: provenance
blockName :: Label -> Name
blockName l = Name ("L" <> Text.pack (show l)) NoPV 

typeOfVar :: Name -> Transpiler Base
typeOfVar x = do
  ctx <- gets typeContext
  case Map.lookup x ctx of
    Just b -> return b
    Nothing -> lift $ throwE $ OtherError ("missing type information for variable " ++ showPretty x) (getPV x)

withContext :: [(Name,Base)] -> Transpiler a -> Transpiler a
withContext g m = do
  ctx <- gets typeContext
  modify' $ \env -> env { typeContext = Map.union ctx (Map.fromList g) }
  x <- m
  
  -- TODO: this just works incidentllay because of the name punning; if we
  --didn't assign a rec lambda var to some let bound var of the same name, we'd
  --never extend the type context for that var 
  
  -- modify' $ \env -> env { typeContext = ctx }
  return x

returnTypeOf :: (Annotated t, HasProvenance a) => Typed t a -> Transpiler PyType
returnTypeOf a = case typeOf a of
  PyType.Callable _ r -> return r
  _ -> lift $ throwE $ OtherError "expected Callable type" (getPV a)

baseTypeFromPyType :: PyType -> Transpiler Base
baseTypeFromPyType = \case
  PyType.Bool -> pure TBool
  PyType.Int  -> pure TInt
  PyType.Str  -> pure TString
  PyType.None -> pure TUnit
  t -> lift $ throwE $ OtherError ("unsupported Python type: " ++ show t) NoPV

------------------------------------------------------------------------------

-- TODO
transpileTopLevel :: HasProvenance a => Typed DomTree a -> Transpiler Program
transpileTopLevel dom = go dom.root
 where
  go l = case dom.nodes ! l of
    FunDef{..} -> do
      funType <- mkFunType _name _args _result
      let ass  = Assume (mangle _name) funType
      let ctx  = collectParams funType
      k       <- withContext ctx $ transpileFun (domTree _body)
      lam     <- mkLambdas _args k
      let def  = Define (mangle _name) lam
      rest    <- go _next
      return   $ ass : def : rest

    Block{..} -> do
      imports <- concat <$> mapM goImport _stmts
      -- TODO: deal with other stmts
      rest    <- go _next
      return   $ imports ++ rest
    
    Branch{} -> do
      lift $ throwE $ OtherError "UnsupportedTopLevel: branch" NoPV -- TODO
    
    BranchFor{} -> do
      lift $ throwE $ OtherError "UnsupportedTopLevel: for" NoPV -- TODO

    Exit -> return []

  goImport = \case
    Py.Import{} -> return [] -- TODO: transpile imports and add to builder environment
    stmt        -> lift $ throwE $ OtherError ("UnsupportedTopLevel: " ++ showPretty stmt) (getPV stmt)

collectParams :: Type -> [(Name,Base)]
collectParams = \case
  TBase _ _ _ _ -> []
  TFun _ t1 t2 _ -> case t1 of
    TBase v b _ _ -> (v,b) : collectParams t2
    TFun _ _ _ _ -> collectParams t2

mkFunType
  :: HasProvenance a 
  => Typed Ident a -> [Typed Parameter a] -> Maybe (Typed Py.Expr a) 
  -> Transpiler Type
mkFunType name args result = do
  retBaseType <- baseTypeFromPyType =<< returnTypeOf name
  let retPV    = maybe (Derived NoPV "inferred") getPV result
  let retType  = TBase dummyName retBaseType (Known PTrue) retPV
  foldM go retType (reverse args)
 where
  go t2 p = case p of
    Param { param_default = Nothing, .. } -> do
      b      <- baseTypeFromPyType (typeOf p)
      let v   = mangle param_name
      let pv  = maybe (Derived NoPV "inferred") getPV param_py_annotation
      let t1  = TBase v b Unknown pv
      return  $ TFun v t1 t2 (getPV p)
  
    Param { param_default = Just expr } ->
      lift $ throwE $ UnsupportedDefaultParameter expr

    _ -> lift $ throwE $ UnsupportedParameter p

mkLambdas :: HasProvenance a => [Typed Parameter a] -> Term -> Transpiler Term 
mkLambdas ps k0 = foldM go k0 (reverse ps)
 where
  go k p = case p of
    Param { param_default = Nothing, .. } -> do
      b      <- baseTypeFromPyType (typeOf p)
      let v   = mangle param_name
      let pv  = maybe (Derived NoPV "inferred") getPV param_py_annotation
      let t   = TBase dummyName b (Known PTrue) pv
      return  $ Lam v t k (getPV p)
  
    Param { param_default = Just expr } ->
      lift $ throwE $ UnsupportedDefaultParameter expr

    _ -> lift $ throwE $ UnsupportedParameter p    

------------------------------------------------------------------------------

transpileFun :: HasProvenance a => Typed DomTree a -> Transpiler Term
transpileFun dom = goDom (Val (Var (blockName dom.root))) dom.root
 where  
  goDom k l = case dom.phiVars ! l of
    [] -> do
      body   <- mkBody l
      e1     <- foldM goDom body (dom.children ! l)
      return  $ Let (blockName l) e1 k NoPV
    
    vs -> do
      body   <- mkBody l
      e1     <- foldM goDom body (dom.children ! l)
      typ    <- mkPhiFunType vs
      lams   <- mkPhiLambdas vs e1
      return  $ Rec (blockName l) typ lams k NoPV
  
  mkBody l = case dom.nodes ! l of
    FunDef{} -> lift $ throwE $ OtherError "nested functions not supported" NoPV -- TODO
    Block{..} -> transpileStmts _stmts =<< mkCall _next
    Branch{..} -> withAtom _cond $ \c -> do
      kTrue  <- mkCall _nextTrue
      kFalse <- mkCall _nextFalse
      return  $ If c kTrue kFalse NoPV
    BranchFor {} -> lift $ throwE $ OtherError "for..in not yet supported" NoPV -- TODO
    Exit -> return $ Val (Con (U NoPV))

  mkCall l = case dom.phiVars ! l of
    [] -> return $ Val (Var (blockName l))
    vs -> return $ foldl' (\e v -> App e v NoPV) (Val (Var (blockName l))) (map (Var . fromString) vs)

mkPhiFunType :: [String] -> Transpiler Type
mkPhiFunType xs = do
  let retBaseType = TUnit -- TODO
  let retType     = TBase dummyName retBaseType (Known PTrue) NoPV
  foldM go retType (reverse xs)
 where
  go t2 x = do
    let v   = fromString x
    b      <- typeOfVar v
    let t1  = TBase v b Unknown NoPV
    return  $ TFun v t1 t2 NoPV

mkPhiLambdas :: [String] -> Term -> Transpiler Term
mkPhiLambdas xs k0 = foldM go k0 (reverse xs)
 where
  go k x = do
    let v   = fromString x
    b      <- typeOfVar v
    let t   = TBase dummyName b (Known PTrue) NoPV
    return  $ Lam v t k NoPV

------------------------------------------------------------------------------

-- TODO: pass along context of all exceptions being caught in the current block
transpileStmts :: HasProvenance a => [Typed Py.Statement a] -> Term -> Transpiler Term
transpileStmts stmts k0 = go stmts
 where
  go []          = return k0
  go (stmt:rest) = case stmt of
    Assign { assign_to = [Py.Var x _], ..} ->
      withTerm assign_expr $ \e1 -> do
        let v   = mangle x
        b      <- baseTypeFromPyType (typeOf assign_expr)
        k      <- withContext [(v,b)] $ go rest
        return  $ Let v e1 k (getPV stmt)

    AnnotatedAssign { ann_assign_to = Py.Var x _, ..} -> do
      let v = mangle x
      b <- baseTypeFromPyType (typeOf x)
      case ann_assign_expr of
        Nothing -> withContext [(v,b)] $ go rest
        Just ex -> withTerm ex $ \e1 -> do
          k <- withContext [(v,b)] $ go rest
          return $ Let v e1 k (getPV stmt)
  
    AugmentedAssign { aug_assign_to = to@(Py.Var x _), ..} -> do
      withAtom aug_assign_expr $ \rhs -> do
        let op = assignOpToOp aug_assign_op
        case axiomForOperator op of
          Nothing -> lift $ throwE $ UnsupportedOperator op
          Just fn -> do
            let f   = Name (fromString fn) (getPV aug_assign_op)        
            let v   = mangle x
            let e1  = mkApp f [Var v, rhs]
            b      <- baseTypeFromPyType (typeOf to)
            k      <- withContext [(v,b)] $ go rest
            return  $ Let v e1 k (getPV stmt)

    -- TODO: technically, assert raises an AssertionError, which could be caught!
    Assert { assert_exprs = [expr] } ->
      withAtom expr $ \a -> do
        let f   = mkApp "assert" [a]
        k      <- go rest
        return  $ Let dummyName f k (getPV stmt)

    _ -> lift $ throwE $ UnsupportedStatement stmt

withTerm :: HasProvenance a => Typed Py.Expr a -> (Term -> Transpiler Term) -> Transpiler Term
withTerm expr k = case expr of
  _ | isAtomic expr -> k =<< Val <$> transpileAtom expr

  Call { call_fun = Py.Var fun _, call_args = ArgExprs args } -> do
    withAtoms args $ \as -> do      
      case axiomForFunction fun.ident_string (map typeOf args) (typeOf expr) of
        Nothing -> k $ mkApp (mangle fun) as
        Just fn -> do
          let f = Name (fromString fn) (getPV fun)
          k $ mkApp f as

  BinaryOp {..} -> do
    withAtom left_op_arg $ \lhs -> do
      withAtom right_op_arg $ \rhs -> do
        case axiomForOperator operator of
          Nothing -> lift $ throwE $ UnsupportedOperator operator
          Just fn -> do
            let f = Name (fromString fn) (getPV operator)
            k $ mkApp f [lhs,rhs]
  
  Subscript {..} -> do
    withAtom subscriptee $ \obj -> do
      withAtom subscript_expr $ \index -> do
        let ts = typeOf subscriptee
        let ti = typeOf subscript_expr
        let tr = typeOf expr
        case axiomForSubscript ts ti tr of
          Nothing -> lift $ throwE $ OtherError "unsupported subscript" (getPV expr)
          Just fn -> do
            let f = Name (fromString fn) (getPV expr)        
            k $ mkApp f [obj,index]

  _ -> lift $ throwE $ UnsupportedExpression expr

mkApp :: Name -> [Atom] -> Term
mkApp f xs = foldl' (\e y -> App e y NoPV) (Val (Var f)) xs

------------------------------------------------------------------------------

withAtom :: HasProvenance a => Typed Py.Expr a -> (Atom -> Transpiler Term) -> Transpiler Term
withAtom expr k
  | isAtomic expr = transpileAtom expr >>= k
  | otherwise = do
      withTerm expr $ \e1 -> do
        v  <- newVar
        b  <- baseTypeFromPyType (typeOf expr)
        e2 <- withContext [(v,b)] $ k (Var v)
        return $ Let v e1 e2 NoPV

withAtoms :: HasProvenance a => [Typed Py.Expr a] -> ([Atom] -> Transpiler Term) -> Transpiler Term
withAtoms xs0 k0 = go [] xs0
 where
  go ys []     = k0 (reverse ys)
  go ys (x:xs) = withAtom x $ \y -> go (y:ys) xs

-- TODO: support global encoding declarations for strings
-- expects expression to be atomic
transpileAtom :: HasProvenance a => Py.Expr a -> Transpiler Atom
transpileAtom expr = case expr of
  Py.Var         {..} -> return $ Var (mangle var_ident)
  Int            {..} -> return $ Con (I int_value (getPV expr))
  LongInt        {..} -> return $ Con (I int_value (getPV expr))
  Float          {}   -> unsupported  -- TODO
  Imaginary      {}   -> unsupported  -- TODO
  Bool           {..} -> return $ Con (B bool_value (getPV expr))
  None           {}   -> return $ Con (U (getPV expr))
  ByteStrings    {}   -> unsupported  -- TODO
  
  Strings {..} -> do 
    case fmap concat $ sequence $ map decodeStringLiteral strings_strings of
      Nothing  -> lift $ throwE $ UnsupportedAtomicExpression expr
      -- Just [c] -> return $ Con (C c (getPV expr))
      Just cs  -> return $ Con (S (Text.pack cs) (getPV expr))

  UnicodeStrings {}   -> unsupported  -- TODO
  Paren          {..} -> transpileAtom paren_expr
  _                   -> panic $ "unexpected non-atomic expression:" <+> pretty expr
 where
  unsupported = lift $ throwE $ UnsupportedAtomicExpression expr

isAtomic :: Py.Expr a -> Bool
isAtomic = \case
  Py.Var         {}   -> True
  Int            {}   -> True
  LongInt        {}   -> True
  Float          {}   -> True
  Imaginary      {}   -> True
  Bool           {}   -> True
  None           {}   -> True
  ByteStrings    {}   -> True
  Strings        {..} -> not $ any isFString strings_strings
  UnicodeStrings {}   -> True
  Paren          {..} -> isAtomic paren_expr
  _                   -> False
