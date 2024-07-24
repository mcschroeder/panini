module Panini.Frontend.Python.Transpiler (transpile) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Foldable
import Data.IntMap.Strict ((!))
import Data.Char
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

transpile :: DomTree -> Either Error Program
transpile dom = runExcept (evalStateT (transpileTopLevel dom) (0))

------------------------------------------------------------------------------

type Transpiler a = StateT (Int) (Except Error) a

-- TODO
transpileTopLevel :: DomTree -> Transpiler Program
transpileTopLevel dom = go dom.root
 where
  go l = case dom.nodes ! l of
    FunDef{..} -> do
      k <- transpileFun (domTree _body)
      lam <- mkLams _args k
      let def = Define (mangle _name) lam
      (def :) <$> go _next

    Block{..} -> do
      imports <- concat <$> mapM goImport _stmts
      (imports ++) <$> go _next
    
    Branch{} -> lift $ throwE $ OtherError "UnsupportedTopLevel: branch" -- TODO
    BranchFor{} -> lift $ throwE $ OtherError "UnsupportedTopLevel: for" -- TODO

    Exit -> return []

  goImport = \case
    Py.Import{} -> return [] -- TODO: transpile imports and add to builder environment
    stmt        -> lift $ throwE $ OtherError $ "UnsupportedTopLevel: " ++ showPretty stmt -- TODO
  
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
        Nothing -> lift $ throwE $ UnsupportedTypeHint typeHint

    Param 
      { param_default = Just param
      } 
      -> lift $ throwE $ UnsupportedDefaultParameter param

    Param 
      { param_py_annotation = Nothing 
      } 
      -> lift $ throwE $ MissingParameterTypeHint p
  
    _ -> lift $ throwE $ UnsupportedParameter p

transpileFun :: DomTree -> Transpiler Term
transpileFun dom = goDom (Val (Var (blockName dom.root))) dom.root
 where  
  goDom k l = case dom.phiVars ! l of
    [] -> do
      body <- mkBody l
      e1 <- foldM goDom body (dom.children ! l)
      return $ Let (blockName l) e1 k NoPV
    
    vs -> do
      body <- mkBody l
      e1 <- foldM goDom body (dom.children ! l)
      let lams = mkPhiLams vs e1
      return $ Rec (blockName l) typeTODO lams k NoPV
  
  mkBody l = case dom.nodes ! l of
    FunDef{} -> lift $ throwE $ OtherError "nested functions not supported" -- TODO

    Block{..} -> do
      k <- mkCall _next
      foldM transpileStmt k (reverse _stmts)

    Branch{..} -> do
      kTrue <- mkCall _nextTrue
      kFalse <- mkCall _nextFalse
      v <- newVar
      let k = If (Var v) kTrue kFalse NoPV
      foldM transpileStmt k ([mkAssignStmt v _cond] :: [StatementSpan])

    BranchFor{} -> lift $ throwE $ OtherError $ "for..in not yet supported" -- TODO
    Exit -> return $ Val (Con (U NoPV))

  mkCall l = case dom.phiVars ! l of
    [] -> return $ Val (Var (blockName l))
    vs -> return $ foldl' (\e v -> App e v NoPV) (Val (Var (blockName l))) (map (Var . fromString) vs)

newVar :: Transpiler Name
newVar = do
  i <- get
  put $ i + 1
  return $ Name (Text.pack $ "v" ++ show i) NoPV

mkAssignStmt :: Name -> ExprSpan -> StatementSpan
mkAssignStmt (Name x _) e = 
  Assign [Py.Var (Ident (Text.unpack x) SpanEmpty) SpanEmpty] e SpanEmpty

mkPhiLams :: [String] -> Term -> Term
mkPhiLams xs k0 = foldr mkPhiLam k0 xs
 where
  mkPhiLam x k = Lam (fromString x) typeTODO k NoPV  -- TODO: type

-- TODO: remove
typeTODO :: Type
typeTODO = TBase dummyName TInt (Known PTrue) NoPV

-- TODO: uniqueness
-- TODO: provenance
blockName :: Label -> Name
blockName l = Name ("L" <> Text.pack (show l)) NoPV 

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


transpileStmt :: Term -> StatementSpan -> Transpiler Term
transpileStmt k = \case
  Assign { assign_to = [Py.Var x _], ..} -> do
    e1 <- transpileExpr assign_expr
    return $ Let (mangle x) e1 k (pySpanToPV stmt_annot)
  
  Assert {..} -> do
    let assertFunc = Py.Var (Ident "assert" stmt_annot) stmt_annot
    fs <- mapM (\e -> transpileSimpleCall assertFunc [e]) assert_exprs
    return $ foldr (\f k' -> Let dummyName f k' NoPV) k fs  -- TODO: pv



  stmt -> lift $ throwE $ UnsupportedStatement stmt


transpileExpr :: ExprSpan -> Transpiler Term
transpileExpr = \case
  expr | isAtomic expr -> Val <$> transpileAtom expr  
  
  Call { call_args = ArgExprs args, .. } -> transpileSimpleCall call_fun args
  
  BinaryOp {..} -> do
    opName <- getOperatorName operator
    let opFunc = Py.Var (Ident opName operator.op_annot) operator.op_annot
    transpileSimpleCall opFunc [left_op_arg,right_op_arg]
  
  -- TODO: this is hardcoded to str.at right now; vary by type!
  Subscript {..} -> do
    let subFunc = Py.Var (Ident "str.at" expr_annot) expr_annot
    transpileSimpleCall subFunc [subscriptee, subscript_expr]

  expr -> lift $ throwE $ UnsupportedExpression expr

transpileSimpleCall :: ExprSpan -> [ExprSpan] -> Transpiler Term
transpileSimpleCall f args = go [] (f:args)
 where
  go vs (x:xs)
    | isAtomic x = do
        v <- transpileAtom x
        go (v:vs) xs

    | otherwise = do
        v <- newVar
        e <- transpileExpr x
        k <- go (Var v : vs) xs
        return $ Let v e k NoPV
  
  go vs [] = case reverse vs of
    z:zs -> return $ foldl' (\e x -> App e x NoPV) (Val z) zs
    _    -> impossible

-- TODO: types: int.eq vs char.eq etc.
getOperatorName :: OpSpan -> Transpiler String
getOperatorName = \case
    LessThan          {} -> return "lt"
    GreaterThan       {} -> return "gt"
    Equality          {} -> return "eq"
    GreaterThanEquals {} -> return "ge"
    LessThanEquals    {} -> return "le"
    NotEquals         {} -> return "ne"
    NotEqualsV2       {} -> return "ne"
    Plus              {} -> return "add"
    Minus             {} -> return "sub"
    op                   -> lift $ throwE $ UnsupportedOperator op

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
  s <- (Text.pack . concat) <$> mapM decode strings_strings
  return $ Con (S s (pySpanToPV expr_annot))
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
