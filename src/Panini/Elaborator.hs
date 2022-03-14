module Panini.Elaborator where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Map (Map)
import Data.Map qualified as Map
import Panini.Checker
import Panini.Error
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

-- TODO: do we need IO in there?

-- | Elaborator monad.
type Panini = StateT PanState (ExceptT Error IO)

throwError :: Error -> Panini ()
throwError = lift . throwE

catchError :: Panini a -> (Error -> Panini a) -> Panini a
catchError = liftCatch catchE

tryError :: Panini a -> Panini (Either Error a)
tryError m = catchError (Right <$> m) (return . Left)

-- | Elaborator state.
data PanState = PanState
  { pan_types :: Ctx            -- ^ global typing context (Gamma)
  , pan_terms :: Map Name Expr  -- ^ top-level functions
  , pan_vcs :: Map Name Con     -- ^ verification conditions
  }

-- | Initial (empty) elaborator state.
initState :: PanState
initState = PanState 
  { pan_types = Map.empty 
  , pan_terms = Map.empty
  , pan_vcs = Map.empty
  }

-------------------------------------------------------------------------------

elabProg :: Prog -> Panini ()
elabProg = mapM_ elabDecl

elabDecl :: Decl -> Panini ()
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
        vc <- lift $ except $ check gamma e t
        modify' $ \ps -> ps {pan_vcs = Map.insert x vc (pan_vcs ps)}
        modify' $ \ps -> ps {pan_terms = Map.insert x e (pan_terms ps)}

solve :: Panini ()
solve = undefined
