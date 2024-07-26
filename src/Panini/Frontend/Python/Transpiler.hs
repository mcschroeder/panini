{-# LANGUAGE RecordWildCards #-}

module Panini.Frontend.Python.Transpiler (transpile) where

import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Char
import Data.Foldable
import Data.IntMap.Strict ((!))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.String
import Data.Text qualified as Text
import Language.Python.Common.SrcLocation
import Panini.Frontend.Python.AST hiding (Var)
import Panini.Frontend.Python.AST qualified as Py
import Panini.Frontend.Python.CFG hiding (Error)
import Panini.Frontend.Python.DomTree
import Panini.Frontend.Python.Error
import Panini.Frontend.Python.Provenance
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

transpile :: DomTree -> Either Error Program
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

mangle :: IdentSpan -> Name
mangle Ident{..} = Name (Text.pack ident_string) (pySpanToPV ident_annot)

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
transpileTopLevel :: DomTree -> Transpiler Program
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

mkFunType :: [ParameterSpan] -> Maybe ExprSpan -> Transpiler Type
mkFunType ps retm = do
  retBaseType <- maybe (pure TUnit) baseTypeFromHint retm
  let retPV    = maybe NoPV (pySpanToPV . expr_annot) retm
  let retType  = TBase dummyName retBaseType (Known PTrue) retPV  
  foldM go retType (reverse ps)
 where
  go t2 p = do
    (x,hint) <- requireOrdinaryHintedParam p
    b        <- baseTypeFromHint hint
    let tpv   = pySpanToPV (annot hint)
    let v     = mangle x
    let t1    = TBase v b Unknown tpv
    let lpv   = pySpanToPV (annot p)
    addTyping v b  -- TODO
    return    $ TFun v t1 t2 lpv

mkLambdas :: [ParameterSpan] -> Term -> Transpiler Term 
mkLambdas ps k0 = foldM go k0 (reverse ps)
 where  
  go k p = do
    (x,hint) <- requireOrdinaryHintedParam p
    b        <- baseTypeFromHint hint
    let tpv   = pySpanToPV (annot hint)
    let t     = TBase dummyName b (Known PTrue) tpv    
    let lpv   = pySpanToPV (annot p)
    let v     = mangle x
    return    $ Lam v t k lpv

requireOrdinaryHintedParam :: ParameterSpan -> Transpiler (IdentSpan, ExprSpan)
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

baseTypeFromHint :: ExprSpan -> Transpiler Base
baseTypeFromHint = \case
  IsVar "bool" -> return TBool
  IsVar "int"  -> return TInt
  IsVar "str"  -> return TString
  expr         -> lift $ throwE $ UnsupportedTypeHint expr

------------------------------------------------------------------------------

transpileFun :: DomTree -> Transpiler Term
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
    Branch{..} -> do      
      kTrue  <- mkCall _nextTrue
      kFalse <- mkCall _nextFalse
      v      <- newVar
      let k   = If (Var v) kTrue kFalse NoPV
      transpileStmts [mkAssignStmt v _cond] k
    
    BranchFor {} -> lift $ throwE $ OtherError $ "for..in not yet supported" -- TODO
    Exit -> return $ Val (Con (U NoPV))

  mkCall l = case dom.phiVars ! l of
    [] -> return $ Val (Var (blockName l))
    vs -> return $ foldl' (\e v -> App e v NoPV) (Val (Var (blockName l))) (map (Var . fromString) vs)

-- TODO: uniqueness
-- TODO: provenance
blockName :: Label -> Name
blockName l = Name ("L" <> Text.pack (show l)) NoPV 

mkAssignStmt :: Name -> ExprSpan -> StatementSpan
mkAssignStmt (Name x _) e = 
  Assign [Py.Var (Ident (Text.unpack x) SpanEmpty) SpanEmpty] e SpanEmpty

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
transpileStmts :: [StatementSpan] -> Term -> Transpiler Term
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
        return $ Let v e1 k (pySpanToPV stmt_annot)

    AnnotatedAssign { ann_assign_to = Py.Var x _, ..} -> do
      b <- baseTypeFromHint ann_assign_annotation
      let v = mangle x
      addTyping v b
      case ann_assign_expr of
        Nothing -> go rest
        Just ex -> withTerm ex $ \e1 -> do
            k <- go rest
            return $ Let v e1 k (pySpanToPV stmt_annot)
  
    -- TODO: technically, assert raises an AssertionError, which could be caught!
    Assert { assert_exprs = [expr], .. } ->
      withAtom expr $ \a -> do
        let f = mkApp "assert" [a]
        k <- go rest
        return $ Let dummyName f k (pySpanToPV stmt_annot)

    _ -> lift $ throwE $ UnsupportedStatement stmt


withTerm :: ExprSpan -> (Term -> Transpiler Term) -> Transpiler Term
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

withAtom :: ExprSpan -> (Atom -> Transpiler Term) -> Transpiler Term
withAtom expr k
  | isAtomic expr = transpileAtom expr >>= k
  | otherwise = do
      withTerm expr $ \e1 -> do
        v  <- newVar
        b  <- typeOfTerm e1
        addTyping v b
        e2 <- k (Var v)
        return $ Let v e1 e2 NoPV

withAtoms :: [ExprSpan] -> ([Atom] -> Transpiler Term) -> Transpiler Term
withAtoms xs0 k0 = go [] xs0
 where
  go ys []     = k0 (reverse ys)
  go ys (x:xs) = withAtom x $ \y -> go (y:ys) xs

mkApp :: Name -> [Atom] -> Term
mkApp f xs = foldl' (\e y -> App e y NoPV) (Val (Var f)) xs



mkOpFun :: OpSpan -> Base -> Base -> Transpiler Name
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


-- expects expression to be atomic
transpileAtom :: ExprSpan -> Transpiler Atom
transpileAtom expr = case expr of
  Py.Var         {..} -> return $ Var (mangle var_ident)
  Int            {..} -> return $ Con (I int_value (pySpanToPV expr_annot))
  LongInt        {..} -> return $ Con (I int_value (pySpanToPV expr_annot))
  Float          {}   -> unsupported  -- TODO
  Imaginary      {}   -> unsupported  -- TODO
  Bool           {..} -> return $ Con (B bool_value (pySpanToPV expr_annot))
  None           {}   -> unsupported  -- TODO
  ByteStrings    {}   -> unsupported  -- TODO
  Strings        {}   -> transpileStringLiteral expr
  UnicodeStrings {}   -> unsupported  -- TODO
  Paren          {..} -> transpileAtom paren_expr
  _                   -> panic $ "unexpected non-atomic expression:" <+> pretty expr
 where
  unsupported = lift $ throwE $ UnsupportedAtomicExpression expr


-- TODO: support triple-quoted strings
-- TODO: support global encoding declarations in source file
-- TODO: check that this really follows the spec
transpileStringLiteral :: ExprSpan -> Transpiler Atom
transpileStringLiteral expr@Strings{..} = do
  let pv = pySpanToPV expr_annot
  s <- concatMapM decode strings_strings  
  case s of
    [c] -> return $ Con (C c pv)
    cs  -> return $ Con (S (Text.pack cs) pv)
 where  
  decode ('r':cs) = unqote cs
  decode ('R':cs) = unqote cs
  decode (    cs) = unescape <$> unqote cs

  unqote ('\"':'\"':'\"':_) = lift $ throwE $ UnsupportedAtomicExpression expr
  unqote ('\"':cs) = pure $ init cs
  unqote ('\'':cs) = pure $ init cs
  unqote _         = panic $ "malformed string literal:" <+> pretty expr

transpileStringLiteral expr =
  panic $ "unexpected expression (not a string literal):" <+> pretty expr

-- TODO: support \N{name}
unescape :: String -> String
unescape ('\'':cs) = unescape cs
unescape ('\"':cs) = unescape cs
unescape ('\\':'\n':cs) = unescape cs
unescape ('\\':'\\':cs) = '\\' : unescape cs
unescape ('\\':'\'':cs) = '\'' : unescape cs
unescape ('\\':'\"':cs) = '\"' : unescape cs
unescape ('\\':'a':cs) = '\a' : unescape cs
unescape ('\\':'b':cs) = '\b' : unescape cs
unescape ('\\':'f':cs) = '\f' : unescape cs
unescape ('\\':'n':cs) = '\n' : unescape cs
unescape ('\\':'r':cs) = '\r' : unescape cs
unescape ('\\':'t':cs) = '\t' : unescape cs
unescape ('\\':'v':cs) = '\v' : unescape cs
unescape ('\\':a:b:c:cs) | Just x <- readOctDigits [a,b,c] = x : unescape cs
unescape ('\\':'x':a:b:cs) | Just x <- readHexDigits [a,b] = x : unescape cs
unescape ('\\':'u':a:b:c:d:cs) | Just x <- readHexDigits [a,b,c,d] = x : unescape cs
unescape ('\\':'U':a:b:c:d:e:f:g:h:cs) | Just x <- readHexDigits [a,b,c,d,e,f,g,h] = x : unescape cs
unescape (c:cs) = c : unescape cs
unescape [] = []

readOctDigits :: [Char] -> Maybe Char
readOctDigits cs = case readLitChar ("\\o" ++ cs) of
  [(c,[])] -> Just c
  _        -> Nothing

readHexDigits :: [Char] -> Maybe Char
readHexDigits cs = case readLitChar ("\\x" ++ cs) of
  [(c,[])] -> Just c
  _        -> Nothing

isAtomic :: ExprSpan -> Bool
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
