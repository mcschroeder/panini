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
import Panini.Frontend.Python.CFG hiding (Error)
import Panini.Frontend.Python.DomTree
import Panini.Frontend.Python.Error
import Panini.Frontend.Python.Strings
import Panini.Panic
import Panini.Pretty
import Panini.Provenance
import Panini.Syntax
import Prelude

------------------------------------------------------------------------------

-- TODO: language-python needs to be updated to support newer Python syntax
-- TODO: be explicit about the Python version that is supported (semantics!)

-- TODO: IDEA: use annot field of Python types to store Python type info
-- TODO: IDEA: replace SrcSpan annot with Panini PV annot?

transpile :: HasProvenance a => DomTree a -> Either Error Program
transpile dom = runExcept (evalStateT (transpileTopLevel dom) env0)
 where
  env0 = TranspilerEnv 
    { varSource   = 0
    , typeContext = Map.fromList [("len", TInt), ("add",TInt), ("sub",TInt), ("lt", TInt), ("charAt", TChar), ("eqChar", TBool)] 
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

typeOfVar :: Name -> Transpiler Base
typeOfVar x = do
  ctx <- gets typeContext
  case Map.lookup x ctx of
    Just b -> return b
    Nothing -> lift $ throwE $ OtherError ("missing type information for variable " ++ showPretty x)

typeOfAtom :: Atom -> Transpiler Base
typeOfAtom (Var x) = typeOfVar x
typeOfAtom (Con c) = return $ typeOfValue c

typeOfTerm :: Term -> Transpiler Base
typeOfTerm e0 = go e0
 where
  go = \case
    Val (Var x) -> do
      ctx <- gets typeContext
      case Map.lookup x ctx of
        Just b -> return b
        Nothing -> lift $ throwE $ OtherError ("missing type information for variable " ++ showPretty x ++ " in " ++ showPretty e0)
    Val (Con c) -> return $ typeOfValue c
    App e _ _ -> go e
    Lam _ _ e _ -> go e
    Let _ _ e _ -> go e
    Rec _ _ _ e _ -> go e
    If _ e _ _ -> go e  

addTyping :: Name -> Base -> Transpiler ()
addTyping x b = do
  ctx <- gets typeContext
  case Map.lookup x ctx of   
    Nothing -> modify' $ \env -> env { typeContext = Map.insert x b ctx }
    Just b1 | b1 == b -> return ()
            | otherwise -> lift $ throwE $ OtherError $ "inconsistent types for " ++ showPretty x

------------------------------------------------------------------------------

-- TODO
transpileTopLevel :: HasProvenance a => DomTree a -> Transpiler Program
transpileTopLevel dom = go dom.root
 where
  go l = case dom.nodes ! l of
    FunDef{..} -> do
      typ     <- mkFunType _args _result
      let ass  = Assume (mangle _name) typ
      k       <- transpileFun (domTree _body)      
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
      lift $ throwE $ OtherError "UnsupportedTopLevel: branch" -- TODO
    
    BranchFor{} -> do
      lift $ throwE $ OtherError "UnsupportedTopLevel: for" -- TODO

    Exit -> return []

  goImport = \case
    Py.Import{} -> return [] -- TODO: transpile imports and add to builder environment
    stmt        -> lift $ throwE $ OtherError $ "UnsupportedTopLevel: " ++ showPretty stmt -- TODO

------------------------------------------------------------------------------

mkFunType :: HasProvenance a => [Parameter a] -> Maybe (Py.Expr a) -> Transpiler Type
mkFunType ps retm = do
  retBaseType <- maybe (pure TUnit) baseTypeFromHint retm
  let retPV    = maybe NoPV getPV retm
  let retType  = TBase dummyName retBaseType (Known PTrue) retPV  
  foldM go retType (reverse ps)
 where
  go t2 p = do
    (x,hint) <- requireOrdinaryHintedParam p
    b        <- baseTypeFromHint hint
    let v     = mangle x
    let t1    = TBase v b Unknown (getPV hint)
    addTyping v b  -- TODO
    return    $ TFun v t1 t2 (getPV p)

mkLambdas :: HasProvenance a => [Parameter a] -> Term -> Transpiler Term 
mkLambdas ps k0 = foldM go k0 (reverse ps)
 where  
  go k p = do
    (x,hint) <- requireOrdinaryHintedParam p
    b        <- baseTypeFromHint hint
    let t     = TBase dummyName b (Known PTrue) (getPV hint)
    let v     = mangle x
    return    $ Lam v t k (getPV p)

requireOrdinaryHintedParam :: HasProvenance a => Parameter a -> Transpiler (Ident a, Py.Expr a)
requireOrdinaryHintedParam p = case p of
  Param
    { param_name
    , param_py_annotation = Just hint
    , param_default       = Nothing
    } 
    -> return (param_name, hint)

  Param { param_default = Just expr } 
    -> lift $ throwE $ UnsupportedDefaultParameter expr

  Param { param_py_annotation = Nothing } 
    -> lift $ throwE $ MissingParameterTypeHint p

  _ -> lift $ throwE $ UnsupportedParameter p

baseTypeFromHint :: HasProvenance a => Py.Expr a -> Transpiler Base
baseTypeFromHint = \case
  IsVar "bool" -> return TBool
  IsVar "int"  -> return TInt
  IsVar "str"  -> return TString
  expr         -> lift $ throwE $ UnsupportedTypeHint expr

------------------------------------------------------------------------------

transpileFun :: HasProvenance a => DomTree a -> Transpiler Term
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
    FunDef{} -> lift $ throwE $ OtherError "nested functions not supported" -- TODO
    Block{..} -> transpileStmts _stmts =<< mkCall _next
    Branch{..} -> withAtom _cond $ \c -> do
      kTrue  <- mkCall _nextTrue
      kFalse <- mkCall _nextFalse
      return  $ If c kTrue kFalse NoPV
    BranchFor {} -> lift $ throwE $ OtherError $ "for..in not yet supported" -- TODO
    Exit -> return $ Val (Con (U NoPV))

  mkCall l = case dom.phiVars ! l of
    [] -> return $ Val (Var (blockName l))
    vs -> return $ foldl' (\e v -> App e v NoPV) (Val (Var (blockName l))) (map (Var . fromString) vs)

-- TODO: uniqueness
-- TODO: provenance
blockName :: Label -> Name
blockName l = Name ("L" <> Text.pack (show l)) NoPV 

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

-- TODO: pass along context of all exceptions being caught in the current block
transpileStmts :: HasProvenance a => [Py.Statement a] -> Term -> Transpiler Term
transpileStmts stmts k0 = go stmts
 where
  go []          = return k0
  go (stmt:rest) = case stmt of
    Assign { assign_to = [Py.Var x _], ..} ->
      withTerm assign_expr $ \e1 -> do
        let v = mangle x
        b <- typeOfTerm e1
        addTyping v b
        k <- go rest
        return $ Let v e1 k (getPV stmt)

    AnnotatedAssign { ann_assign_to = Py.Var x _, ..} -> do
      b <- baseTypeFromHint ann_assign_annotation
      let v = mangle x
      addTyping v b
      case ann_assign_expr of
        Nothing -> go rest
        Just ex -> withTerm ex $ \e1 -> do
            k <- go rest
            return $ Let v e1 k (getPV stmt)
  
    -- TODO: technically, assert raises an AssertionError, which could be caught!
    Assert { assert_exprs = [expr] } ->
      withAtom expr $ \a -> do
        let f = mkApp "assert" [a]
        k <- go rest
        return $ Let dummyName f k (getPV stmt)

    _ -> lift $ throwE $ UnsupportedStatement stmt


withTerm :: HasProvenance a => Py.Expr a -> (Term -> Transpiler Term) -> Transpiler Term
withTerm expr k = case expr of
  _ | isAtomic expr -> k =<< Val <$> transpileAtom expr

  -- TODO: transpile known functions  
  Call { call_fun = Py.Var f _, call_args = ArgExprs args } -> do
    withAtoms args $ \as -> do
      k $ mkApp (mangle f) as

  BinaryOp {..} -> do
    withAtom left_op_arg $ \lhs -> do
      withAtom right_op_arg $ \rhs -> do
        b1 <- typeOfAtom lhs
        b2 <- typeOfAtom rhs        
        op <- mkOpFun operator b1 b2
        k $ mkApp op [lhs,rhs]
  
  Subscript {..} -> do
    withAtom subscriptee $ \obj -> do
      withAtom subscript_expr $ \index -> do
        b1 <- typeOfAtom obj
        b2 <- typeOfAtom index
        fn <- mkSubscriptFun b1 b2
        k $ mkApp fn [obj,index]

  _ -> lift $ throwE $ UnsupportedExpression expr

withAtom :: HasProvenance a => Py.Expr a -> (Atom -> Transpiler Term) -> Transpiler Term
withAtom expr k
  | isAtomic expr = transpileAtom expr >>= k
  | otherwise = do
      withTerm expr $ \e1 -> do
        v  <- newVar
        b  <- typeOfTerm e1
        addTyping v b
        e2 <- k (Var v)
        return $ Let v e1 e2 NoPV

withAtoms :: HasProvenance a => [Py.Expr a] -> ([Atom] -> Transpiler Term) -> Transpiler Term
withAtoms xs0 k0 = go [] xs0
 where
  go ys []     = k0 (reverse ys)
  go ys (x:xs) = withAtom x $ \y -> go (y:ys) xs

mkApp :: Name -> [Atom] -> Term
mkApp f xs = foldl' (\e y -> App e y NoPV) (Val (Var f)) xs



mkOpFun :: HasProvenance a => Op a -> Base -> Base -> Transpiler Name
mkOpFun LessThan{}          TInt    TInt    = return "lt"
mkOpFun GreaterThan{}       TInt    TInt    = return "gt"
mkOpFun Equality{}          TInt    TInt    = return "eq"
mkOpFun Equality{}          TString TString = return "match"
mkOpFun Equality{}          TChar   TChar   = return "eqChar"
mkOpFun GreaterThanEquals{} TInt    TInt    = return "ge"
mkOpFun LessThanEquals{}    TInt    TInt    = return "le"
mkOpFun Plus{}              TInt    TInt    = return "add"
mkOpFun Minus{}             TInt    TInt    = return "sub"
mkOpFun op                   _      _       = lift $ throwE $ UnsupportedOperator op -- TODO: type-specific error message

mkSubscriptFun :: Base -> Base -> Transpiler Name
mkSubscriptFun TString TInt = return "charAt"
mkSubscriptFun a b = lift $ throwE $ OtherError $ "unsupported subscript" ++ showPretty a ++ " " ++ showPretty b


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
  None           {}   -> unsupported  -- TODO
  ByteStrings    {}   -> unsupported  -- TODO
  
  Strings {..} -> do 
    case fmap concat $ sequence $ map decodeStringLiteral strings_strings of
      Nothing  -> lift $ throwE $ UnsupportedAtomicExpression expr
      Just [c] -> return $ Con (C c (getPV expr))
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

isFString :: String -> Bool
isFString ('f':_)     = True
isFString ('F':_)     = True
isFString ('r':'f':_) = True
isFString ('r':'F':_) = True
isFString ('R':'f':_) = True
isFString ('R':'F':_) = True
isFString _           = False
