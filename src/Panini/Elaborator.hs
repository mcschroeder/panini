module Panini.Elaborator where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Map (Map)
import Data.Map qualified as Map
import Panini.Error
import Panini.Syntax
import Panini.TypeChecker
import Prelude

-------------------------------------------------------------------------------

-- | Elaborator monad.
type Elab = StateT ElabState (ExceptT Error IO)

-- | Throw an `Error` in the elaborator monad.
throwError :: Error -> Elab ()
throwError = lift . throwE

-- | Catch an `Error` in the elaborator monad.
catchError :: Elab a -> (Error -> Elab a) -> Elab a
catchError = liftCatch catchE

-- | Try an elaborator action and return any thrown `Error`.
tryError :: Elab a -> Elab (Either Error a)
tryError m = catchError (Right <$> m) (return . Left)

-- | Elaborator state.
data ElabState = ElabState
  { pan_types :: Ctx            -- ^ global typing context (Gamma)
  , pan_terms :: Map Name Term  -- ^ top-level functions
  , pan_vcs :: Map Name Con     -- ^ verification conditions
  }

-- | Initial (empty) elaborator state.
initState :: ElabState
initState = ElabState 
  { pan_types = Map.empty 
  , pan_terms = Map.empty
  , pan_vcs = Map.empty
  }

-------------------------------------------------------------------------------

elabProg :: Prog -> Elab ()
elabProg = mapM_ elabDecl

elabDecl :: Decl -> Elab ()
elabDecl (Assume x t) = do
  gamma <- gets pan_types
  case Map.lookup x gamma of
    Just _ -> throwError $ AlreadyDefined x
    Nothing -> do
      modify' $ \ps -> ps { pan_types = Map.insert x t (pan_types ps)}

elabDecl (Define x e) = do
  gamma <- gets pan_types
  terms <- gets pan_terms
  case Map.lookup x terms of
    Just _ -> throwError $ AlreadyDefined x
    Nothing -> case Map.lookup x gamma of
      Nothing -> throwError $ MissingType x
      Just t -> do
        (t', vc) <- lift $ except $ runTC $ do          
          t' <- fresh gamma t
          let gamma' = Map.insert x t' gamma
          vc <- check gamma' e t'
          return (t',vc)
        modify' $ \ps -> ps {pan_types = Map.insert x t' (pan_types ps)}
        modify' $ \ps -> ps {pan_vcs = Map.insert x vc (pan_vcs ps)}
        modify' $ \ps -> ps {pan_terms = Map.insert x e (pan_terms ps)}

solve :: Elab ()
solve = undefined
