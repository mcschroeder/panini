{-# LANGUAGE RecordWildCards #-}

module Panini.Frontend.Python.Transpiler (transpile) where

import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Foldable
import Data.IntMap.Strict ((!))
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Data.Text qualified as Text
import Panini.Frontend.Python.AST hiding (Var)
import Panini.Frontend.Python.AST qualified as Py
import Panini.Frontend.Python.Axioms
import Panini.Frontend.Python.CFG hiding (Error)
import Panini.Frontend.Python.DomTree
import Panini.Frontend.Python.Error
import Panini.Frontend.Python.Pretty ()
import Panini.Frontend.Python.Provenance ()
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
transpile dom = runExcept $ do
  let env0 = TranspilerEnv 0 mempty
  (prog, env) <- runStateT (transpileTopLevel dom) env0
  let preamble = [Assume (fromString f) t | (f,t) <- Set.toAscList env.axioms]
  return $ preamble ++ prog

------------------------------------------------------------------------------

data TranspilerEnv = TranspilerEnv
  { varSource :: Int
  , axioms    :: Set Axiom
  }
  deriving stock (Show)

type Transpiler a = StateT TranspilerEnv (Except Error) a

newVar :: Transpiler Name
newVar = do
  i <- gets varSource
  modify' $ \env -> env { varSource = i + 1 }
  return $ Name (Text.pack $ "v" ++ show i) NoPV

addAxiom :: Axiom -> Transpiler ()
addAxiom ax = modify' $ \env -> env { axioms = Set.insert ax env.axioms }

mangle :: HasProvenance annot => Ident annot -> Name
mangle x = Name (Text.pack x.ident_string) (getPV x)

-- TODO: uniqueness
-- TODO: provenance
blockName :: Label -> Name
blockName l = Name ("L" <> Text.pack (show l)) NoPV 

returnTypeOf :: (Functor t, Annotated t, HasProvenance a) => Typed t a -> Transpiler PyType
returnTypeOf a = case typeOf a of
  PyType.Callable _ r -> return r
  _ -> lift $ throwE $ OtherError "expected Callable type" (getPV a)

baseTypeFromPyType :: PV -> PyType -> Transpiler Base
baseTypeFromPyType pv = \case
  PyType.Bool -> pure TBool
  PyType.Int  -> pure TInt
  PyType.Str  -> pure TString
  PyType.None -> pure TUnit
  t -> lift $ throwE $ OtherError ("unsupported Python type: " ++ showPretty t) pv

finalBaseReturnType :: Type -> Base
finalBaseReturnType = \case
  TBase _ b _ _ -> b
  TFun _ _ t _  -> finalBaseReturnType t

------------------------------------------------------------------------------

-- TODO
transpileTopLevel :: HasProvenance a => Typed DomTree a -> Transpiler Program
transpileTopLevel dom = go dom.root
 where
  go l = ensureNoExcept (dom.nodes ! l) >>= \case
    FunDef{..} -> do
      funType <- mkFunType _name _args _result
      let ass  = Assume (mangle _name) funType
      k       <- transpileFun (finalBaseReturnType funType) (domTree _body)
      lam     <- mkLambdas _args k
      let def  = Define (mangle _name) lam
      rest    <- go _next
      return   $ ass : def : rest

    Block{..} -> do
      imports <- concatMapM goImport $ filter (not . isDocstring) _stmts
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

  isDocstring = \case
    StmtExpr{stmt_expr = Strings{}} -> True
    _                               -> False

-- TODO
ensureNoExcept :: Node a -> Transpiler (Node a)
ensureNoExcept Exit = return Exit
ensureNoExcept node = case _except node of
  [] -> return node
  _  -> lift $ throwE $ OtherError "except clauses not yet supported" NoPV

mkFunType
  :: HasProvenance a 
  => Typed Ident a -> [Typed Parameter a] -> Maybe (Typed Py.Expr a) 
  -> Transpiler Type
mkFunType name args result = do
  retBaseType <- baseTypeFromPyType (getPV name) =<< returnTypeOf name
  let retPV    = maybe (Derived NoPV "inferred") getPV result
  let retType  = TBase dummyName retBaseType (Known PTrue) retPV
  foldM go retType (reverse args)
 where
  go t2 p = case p of
    Param { param_default = Nothing, .. } -> do
      b      <- baseTypeFromPyType (getPV p) (typeOf p)
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
      b      <- baseTypeFromPyType (getPV p) (typeOf p)
      let v   = mangle param_name
      let pv  = maybe (Derived NoPV "inferred") getPV param_py_annotation
      let t   = TBase dummyName b (Known PTrue) pv
      return  $ Lam v t k (getPV p)
  
    Param { param_default = Just expr } ->
      lift $ throwE $ UnsupportedDefaultParameter expr

    _ -> lift $ throwE $ UnsupportedParameter p    

------------------------------------------------------------------------------

transpileFun :: HasProvenance a => Base -> Typed DomTree a -> Transpiler Term
transpileFun returnType dom = do
  k <- mkCall dom.root
  goDom k dom.root
 where  
  goDom k l = case dom.phiVars ! l of
    vs -> do
      body   <- mkBody l
      e1     <- foldM goDom body (reverse $ dom.children ! l)
      typ    <- mkPhiFunType vs returnType
      lams   <- mkPhiLambdas vs e1
      return  $ Rec (blockName l) typ lams k NoPV
  
  mkBody l = ensureNoExcept (dom.nodes ! l) >>= \case
    Block{..} -> transpileStmts returnType _stmts =<< mkCall _next

    Branch{..} -> withAtom _cond $ \c -> do
      kTrue  <- mkCall _nextTrue
      kFalse <- mkCall _nextFalse
      return  $ If c kTrue kFalse NoPV

    -- TODO: there should be a more elegant way to solve this
    Exit -> case returnType of
      TUnit -> return $ Val (Con (U NoPV))
      TBool -> return $ Val (Con (B False NoPV))
      TInt  -> return $ Val (Con (I 0 NoPV))
      TChar -> return $ Val (Con (C '\NUL' NoPV))
      TString -> return $ Val (Con (S "" NoPV))

    FunDef{} -> lift $ throwE $ OtherError "nested functions not supported" NoPV -- TODO
    BranchFor {} -> lift $ throwE $ OtherError "for..in not yet supported" NoPV -- TODO

  mkCall l = case dom.phiVars ! l of
    [] -> return $ Val (Var (blockName l))
    vs -> return $ mkApp (blockName l) (map (Var . fromString . fst) vs)

mkPhiFunType :: [(String,PyType)] -> Base -> Transpiler Type
mkPhiFunType xs retBaseType = do
  let retType = TBase dummyName retBaseType (Known PTrue) NoPV
  foldM go retType (reverse xs)
 where
  go t2 (x,xt) = do
    let v   = fromString x
    b      <- baseTypeFromPyType NoPV xt
    let t1  = TBase v b Unknown NoPV
    return  $ TFun v t1 t2 NoPV

mkPhiLambdas :: [(String,PyType)] -> Term -> Transpiler Term
mkPhiLambdas xs k0 = foldM go k0 (reverse xs)
 where
  go k (x,xt) = do
    let v   = fromString x
    b      <- baseTypeFromPyType NoPV xt
    let t   = TBase dummyName b (Known PTrue) NoPV
    return  $ Lam v t k NoPV

------------------------------------------------------------------------------

-- TODO: pass along context of all exceptions being caught in the current block
transpileStmts :: HasProvenance a => Base -> [Typed Py.Statement a] -> Term -> Transpiler Term
transpileStmts returnType stmts k0 = go stmts
 where
  go []          = return k0
  go (stmt:rest) = case stmt of
    Assign { assign_to = [Py.Var x _], ..} ->
      withTerm assign_expr $ \e1 -> do
        let v   = mangle x
        k      <- go rest
        return  $ Let v e1 k (getPV stmt)
    
    Assign { assign_to = [Tuple (expectVars -> Just xs) _], ..}
      | typeOf assign_expr == PyType.Str
      , all (== PyType.Str) (map typeOf xs) -> do
        withAtom assign_expr $ \str -> do
          withAssertStringLength str (length xs) (getPV stmt) $ do
            let vs = map mangle xs
            k <- go rest
            foldrM (mkSubscriptChain str) k (zip vs [0..])
     where
      mkSubscriptChain str (v,i) e2 = do
        let argTys  = [PyType.Str, PyType.Int]
        let retTy   = PyType.Str
        let pv      = getPV stmt
        strAt      <- getAxiom "__getitem__" argTys retTy pv
        let index   = Con (I i (getPV stmt))
        let e1      = mkApp strAt [str,index]
        return      $ Let v e1 e2 (getPV stmt)

    AnnotatedAssign { ann_assign_to = Py.Var x _, ..} -> do      
      case ann_assign_expr of
        Nothing -> go rest
        Just ex -> withTerm ex $ \e1 -> do
          let v   = mangle x
          k      <- go rest
          return $ Let v e1 k (getPV stmt)
  
    AugmentedAssign { aug_assign_to = to_expr@(Py.Var x _), ..} -> do
      let op     = assignOpToOp aug_assign_op
      let fun    = desugarBinaryOp op
      let args   = [to_expr, aug_assign_expr]
      let tyArgs = map typeOf args
      let retTy  = typeOf aug_assign_expr
      e1        <- applyAxiom fun args tyArgs retTy (getPV aug_assign_op)
      e2        <- go rest
      return     $ Let (mangle x) e1 e2 (getPV stmt)

    -- TODO: technically, assert raises an AssertionError, which could be caught!
    Assert { assert_exprs = [expr] } -> do
      e1 <- applyAxiom "assert" [expr] [typeOf expr] PyType.None (getPV stmt)
      e2 <- go rest
      return $ Let dummyName e1 e2 (getPV stmt)

    Return {..} -> case return_expr of
      Nothing -> return $ Val $ Con $ U $ getPV stmt
      Just ex -> withTerm ex return
    
    -- strip docstrings and other free-standing string literals
    StmtExpr { stmt_expr = Strings{} } -> go rest

    Raise {} -> do
      let ax@(fn,_) = assertWithType returnType
      addAxiom ax
      let f = Name (fromString fn) (getPV stmt)
      let false_ = Py.Bool False (Nothing, NoPV)
      withAtoms [false_] $ return . mkApp f

    _ -> lift $ throwE $ UnsupportedStatement stmt

withTerm :: HasProvenance a => Typed Py.Expr a -> (Term -> Transpiler Term) -> Transpiler Term
withTerm expr k = case expr of
  _ | isAtomic expr -> k =<< Val <$> transpileAtom expr

  Call { call_fun = Py.Var fun _, call_args = ArgExprs args } -> do
    let tyArgs = map typeOf args
    k =<< applyAxiom fun.ident_string args tyArgs (typeOf expr) (getPV fun)
  
  Call { call_fun = Dot {..}, call_args = ArgExprs args } -> do
    let fun    = dot_attribute.ident_string
    let args'  = dot_expr : args
    let tyArgs = map typeOf args'
    let pv     = getPV dot_attribute
    k =<< applyAxiom fun args' tyArgs (typeOf expr) pv
  
  BinaryOp { operator = Py.NotIn {..}, .. } -> do
    let expr' = BinaryOp { operator = Py.In {..}, .. }
    k =<< applyAxiom "not" [expr'] [typeOf expr'] PyType.Bool (getPV expr)

  BinaryOp { operator = IsNot {..}, ..} -> do
    let expr' = BinaryOp { operator = Is {..}, .. }
    k =<< applyAxiom "not" [expr'] [typeOf expr'] PyType.Bool (getPV expr)
  
  BinaryOp {..} -> do
    let opFun  = desugarBinaryOp operator
    let args   = [left_op_arg, right_op_arg]
    let tyArgs = map typeOf args
    let tyExpr = typeOf expr
    let pv     = getPV operator
    k =<< applyAxiom opFun args tyArgs tyExpr pv
  
  UnaryOp { operator = op@(Minus {}), op_arg = Int {..} } ->
    k $ Val (Con (I (- int_value) (getPV op)))

  UnaryOp {..} -> do
    let opFun  = desugarUnaryOp operator
    let arg    = [op_arg]
    let tyArg  = [typeOf op_arg]
    let tyExpr = typeOf expr
    let pv     = getPV operator
    k =<< applyAxiom opFun arg tyArg tyExpr pv

  Subscript {..} -> do
    let args   = [subscriptee, subscript_expr]
    let tyArgs = map typeOf args
    k =<< applyAxiom "__getitem__" args tyArgs (typeOf expr) (getPV expr)

  SlicedExpr { slices = [SliceProper{..}], .. } -> do
    let tyObj    = typeOf slicee
    let tyLower  = maybe PyType.None typeOf slice_lower
    let tyUpper  = maybe PyType.None typeOf slice_upper
    let tyStride = maybe [] (pure . maybe PyType.None typeOf) slice_stride
    let tyArgs   = [tyObj, tyLower, tyUpper] ++ tyStride
    let tyItem   = typeOf expr
    let args     = slicee : catMaybes [slice_lower, slice_upper, join slice_stride]
    k =<< applyAxiom "__getitem__" args tyArgs tyItem (getPV expr)

  SlicedExpr { slices = [SliceExpr{..}], .. } -> do
    let tyObj    = typeOf slicee
    let tyArgs   = [tyObj, typeOf slice_expr]
    let tyItem   = typeOf expr
    let args     = [slicee, slice_expr]
    k =<< applyAxiom "__getitem__" args tyArgs tyItem (getPV expr)

  _ -> lift $ throwE $ UnsupportedExpression expr

applyAxiom
  :: HasProvenance a 
  => String -> [Typed Py.Expr a] -> [PyType] -> PyType -> PV 
  -> Transpiler Term
applyAxiom fun args argTys retTy pv = do
  f <- getAxiom fun argTys retTy pv
  withAtoms args $ return . mkApp f

getAxiom :: String -> [PyType] -> PyType -> PV -> Transpiler Name
getAxiom fun argTys retTy pv = do
  case axiomForFunction fun argTys retTy of
    Nothing -> lift $ throwE $ MissingAxiom fun argTys retTy pv
    Just ax@(fn,_) -> do
      addAxiom ax
      return $ Name (fromString fn) pv

mkApp :: Name -> [Atom] -> Term
mkApp f xs = foldl' (\e y -> App e y NoPV) (Val (Var f)) xs

-- TODO: raises AssertionError, which could be caught
withAssertStringLength 
  :: Integral a 
  => Atom -> a -> PV -> Transpiler Term -> Transpiler Term
withAssertStringLength str n pv k = do  
  assertFun <- getAxiom "assert" [PyType.Bool] PyType.None pv
  lengthFun <- getAxiom "len" [PyType.Str] PyType.Int pv
  eqFun     <- getAxiom "__eq__" [PyType.Int,PyType.Int] PyType.Bool pv
  strLength <- newVar
  predicate <- newVar
  let expN   = Con (I (fromIntegral n) pv)
  e4        <- k
  let e3     = Let dummyName (mkApp assertFun [Var predicate]) e4 pv
  let e2     = Let predicate (mkApp eqFun [Var strLength, expN]) e3 pv
  return     $ Let strLength (mkApp lengthFun [str]) e2 pv

------------------------------------------------------------------------------

withAtom :: HasProvenance a => Typed Py.Expr a -> (Atom -> Transpiler Term) -> Transpiler Term
withAtom expr k
  | isAtomic expr = transpileAtom expr >>= k
  | otherwise = do
      withTerm expr $ \e1 -> do
        v  <- newVar
        e2 <- k (Var v)
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

------------------------------------------------------------------------------

desugarBinaryOp :: Op a -> String
desugarBinaryOp = \case
  And               {} -> "and"
  Or                {} -> "or"
  Exponent          {} -> "__pow__"
  LessThan          {} -> "__lt__"
  GreaterThan       {} -> "__gt__"
  Equality          {} -> "__eq__"
  GreaterThanEquals {} -> "__ge__"
  LessThanEquals    {} -> "__le__"
  NotEquals         {} -> "__ne__"
  NotEqualsV2       {} -> "__ne__"
  Py.In             {} -> "__contains__"
  Is                {} -> "is"
  BinaryOr          {} -> "__or__"
  Xor               {} -> "__xor__"
  BinaryAnd         {} -> "__and__"
  ShiftLeft         {} -> "__lshift__"
  ShiftRight        {} -> "__rshift__"
  Multiply          {} -> "__mul__"
  Plus              {} -> "__add__"
  Minus             {} -> "__sub__"
  Divide            {} -> "__truediv__"
  FloorDivide       {} -> "__floordiv__"
  MatrixMult        {} -> "__matmul__"
  Modulo            {} -> "__mod__"
  op -> panic $ "desugarBinaryOp: unexpected operator:" <+> pretty op

desugarUnaryOp :: Op a -> String
desugarUnaryOp = \case
  Plus   {} -> "__pos__"
  Minus  {} -> "__neg__"
  Invert {} -> "__invert__"
  Not    {} -> "not"
  op -> panic $ "desugarUnaryOp: unexpected operator:" <+> pretty op
