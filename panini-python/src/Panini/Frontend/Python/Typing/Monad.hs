module Panini.Frontend.Python.Typing.Monad where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Either
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Panini.Frontend.Python.AST
import Panini.Frontend.Python.Pretty ()
import Panini.Frontend.Python.Typing.PyType
import Panini.Panic
import Panini.Pretty
import Panini.Provenance
import Prelude

------------------------------------------------------------------------------

type Infer a = ExceptT TypeError (State Env) a

runInfer :: Infer a -> Either TypeError a
runInfer m = evalState (runExceptT m) (Env 0 mempty [] mempty)

data TypeError 
  = InfiniteType PyType PyType
  | CannotSolve Constraint
  | forall a. HasProvenance a => UnsupportedTypeHint (Expr a)
  | forall a. HasProvenance a => UnsupportedParam (Parameter a)
  | forall a. HasProvenance a => UnsupportedArg (Argument a)
  | UnknownFunction String PV

instance Pretty TypeError where
  pretty = \case
    InfiniteType a b -> "infinite type:" <+> pretty a <+> "occurs in" <+> pretty b
    CannotSolve c -> group $ nest 2 $ "cannot solve constraint:" <\> pretty c
    UnsupportedTypeHint e -> "unsupported type hint:" <+> pretty e 
    UnsupportedParam p -> "unsupported parameter:" <+> pretty p
    UnsupportedArg a -> "unsupported argument:" <+> pretty a
    UnknownFunction f _ -> "unknown function:" <+> pretty f

instance HasProvenance TypeError where
  getPV = \case
    InfiniteType _ _ -> NoPV
    CannotSolve c -> getPV c
    UnsupportedTypeHint a -> getPV $ annot a
    UnsupportedParam a -> getPV $ annot a
    UnsupportedArg a -> getPV $ annot a
    UnknownFunction _ pv -> pv

data Env = Env
  { metaVarCount    :: Int
  , varContext      :: Map String PyType
  , returnTypeStack :: [[PyType]]
  , subConstraints  :: Set Constraint
  }
  deriving stock (Show)

------------------------------------------------------------------------------

newMetaVar :: Infer PyType
newMetaVar = do
  i <- lift $ gets metaVarCount
  lift $ modify' (\e -> e { metaVarCount = i + 1 })
  return (MetaVar i)

typeOfVar :: String -> Infer (Maybe PyType)
typeOfVar x = lift $ Map.lookup x <$> gets varContext

------------------------------------------------------------------------------

registerVar :: String -> PyType -> Infer ()
registerVar x t = lift $
  modify' $ \e -> e { varContext = Map.insert x t e.varContext }

------------------------------------------------------------------------------

pushEmptyReturnTypeStackFrame :: Infer ()
pushEmptyReturnTypeStackFrame = lift $
  modify' $ \e -> e { returnTypeStack = [] : e.returnTypeStack }

addReturnTypeToStackFrame :: PyType -> Infer ()
addReturnTypeToStackFrame t = lift $ gets returnTypeStack >>= \case
  [] -> panic $ "empty return type stack"
  (x:xs) -> do
    modify' $ \e -> e { returnTypeStack = (t:x) : xs }

popReturnTypeStackFrame :: Infer [PyType]
popReturnTypeStackFrame = lift $ gets returnTypeStack >>= \case
  [] -> panic $ "empty return type stack"
  (x:xs) -> do
    modify' $ \e -> e { returnTypeStack = xs }
    return x

------------------------------------------------------------------------------

data Constraint = Sub PyType PyType PV | SubChoice [PyType] PyType PV
  deriving stock (Eq, Ord, Show, Read)

pattern (:≤) :: PyType -> PyType -> Constraint
pattern a :≤ b <- Sub a b _ where
  a :≤ b = Sub a b NoPV

pattern (:*≤) :: [PyType] -> PyType -> Constraint
pattern a :*≤ b <- SubChoice a b _ where
  a :*≤ b = SubChoice a b NoPV

{-# COMPLETE (:≤), (:*≤) #-}

instance Pretty Constraint where
  pretty (a :≤ b) = pretty a <+> "≤" <+> pretty b
  pretty (a :*≤ b) = pretty a <+> "*≤" <+> pretty b 

instance HasProvenance Constraint where
  getPV = \case
    Sub _ _ pv -> pv
    SubChoice _ _ pv -> pv

constrain :: Constraint -> Infer ()
constrain c = do
  lift $ modify' $ \e -> e { subConstraints = Set.insert c e.subConstraints }

constrainPV :: HasProvenance a => a -> Constraint -> Infer ()
constrainPV s c = constrain c'
 where
  pv = getPV s
  c' = case c of
    Sub a b _ -> Sub a b pv
    SubChoice a b _ -> SubChoice a b pv

------------------------------------------------------------------------------

-- | Restores the environment if the 'Infer' action fails.
try :: Infer a -> Infer a
try act = do
  env <- lift get
  act `catchE` (\err -> lift (put env) >> throwE err)

-- | Tries all given 'Infer' actions, in order, returning the results of those
-- that succeed without error.
tryAll :: [Infer a] -> Infer [a]
tryAll = go []
 where
  go ys    []  = return $ reverse ys
  go ys (x:xs) = tryE (try x) >>= \case
    Left  _ -> go    ys  xs
    Right y -> go (y:ys) xs
