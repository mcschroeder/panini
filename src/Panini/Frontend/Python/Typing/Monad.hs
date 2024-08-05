module Panini.Frontend.Python.Typing.Monad where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Either
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Panini.Frontend.Python.Typing.Builtins
import Panini.Frontend.Python.Typing.PyType
import Prelude

------------------------------------------------------------------------------

type Infer a = ExceptT Error (State Env) a

runInfer :: Infer a -> Either Error a
runInfer m = evalState (runExceptT m) (Env 0 mempty builtinFunctions [])

data Error 
  = InfiniteType PyType PyType
  | CannotUnify PyType PyType
  | UnsupportedTypeHint -- TODO (Expr a)
  deriving stock (Show)

data Env = Env
  { metaVarCount   :: Int
  , metaVarContext :: IntMap PyType
  , varContext     :: Map String PyType
  , returnTypeStack :: [PyType]
  }
  deriving stock (Show)

------------------------------------------------------------------------------

newMetaVar :: Infer PyType
newMetaVar = do
  i <- lift $ gets metaVarCount
  lift $ modify' (\e -> e { metaVarCount = i + 1 })
  return (MetaVar i)

lookupMetaVar :: Int -> Infer (Maybe PyType)
lookupMetaVar i = lift $ IntMap.lookup i <$> gets metaVarContext

setMetaVar :: Int -> PyType -> Infer ()
setMetaVar i t = lift $  
  modify' $ \e -> e { metaVarContext = IntMap.insert i t e.metaVarContext }

typeOfVar :: String -> Infer (Maybe PyType)
typeOfVar x = lift $ Map.lookup x <$> gets varContext

registerVar :: String -> PyType -> Infer ()
registerVar x t = lift $
  modify' $ \e -> e { varContext = Map.insert x t e.varContext }

pushReturnType :: PyType -> Infer ()
pushReturnType t = lift $ 
  modify' $ \e -> e { returnTypeStack = t : e.returnTypeStack }

peekReturnType :: Infer (Maybe PyType)
peekReturnType = lift $ listToMaybe <$> gets returnTypeStack

popReturnType :: Infer (Maybe PyType)
popReturnType = lift $ gets returnTypeStack >>= \case
  []     -> return Nothing
  (x:ys) -> do
    modify' $ \e -> e { returnTypeStack = ys }
    return $ Just x

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
