{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}

module Panini.Elab
  ( elaborate
  , assume
  , define
  , import_
  , envToContext -- TODO: weird place for this?
  ) where

import Algebra.Lattice
import Control.Monad.Extra
import Control.Monad.Trans.State.Strict
import Data.Map qualified as Map
import Data.Maybe
import Data.Text.IO qualified as Text
import Panini.Environment
import Panini.Error
import Panini.Infer
import Panini.Modules
import Panini.Monad
import Panini.Parser
import Panini.Pretty
import Panini.Provenance
import Panini.Solver
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

-- | Retrieve a definition from the environment.
envLookup :: Name -> Pan (Maybe Definition)
envLookup x = Map.lookup x <$> gets environment

-- | Extend the environment with a new definition.
envExtend :: Name -> Definition -> Pan ()
envExtend x d = modify' $ \s -> s 
  { environment = Map.insert x d s.environment
  }

-- TODO: do we need this?
-- -- | Remove a definition from the environment.
-- envDelete :: Name -> Pan ()
-- envDelete x = modify' $ \s -> s { environment = Map.delete x s.environment }

-- | Convert an elaborator environment to a typechecking context by throwing
-- away all non-final definitions.
envToContext :: Environment -> Context 
envToContext = Map.map go
  where
    go (Assumed {_type}) = _type
    go (Verified {_solvedType}) = _solvedType

-------------------------------------------------------------------------------

-- | Elaborate a program by elaborating all of its statments and updating the
-- environment accordingly. If elaboration fails at any point, an error is
-- thrown and any intermittent changes to the environment are rolled back.
elaborate :: Module -> Program -> Pan ()
elaborate thisModule prog = do
  env0 <- get
  unless (thisModule == replModule) $
    when (thisModule `elem` env0.loadedModules) $ do
      panic "reloading modules is not yet implemented" -- TODO
  tryError (mapM_ elab prog) >>= \case
    Right () -> do
      unless (thisModule == replModule) $
        modify' $ \s -> s { loadedModules = thisModule : s.loadedModules }
    Left err -> do
      put env0
      throwError err
 where
  elab = \case
    Assume x t  -> assume x t
    Define x e  -> define x e
    Import fp _ -> import_ $ moduleRelativeTo thisModule fp
                      -- TODO: add provenance to error

-- | Add an assumed type to the environment.
assume :: Name -> Type -> Pan ()
assume x t = do
  logMessage $ "Assume" <+> pretty x <+> ":" <+> pretty t
  whenJustM (envLookup x) $ \_ -> throwError $ AlreadyDefined x -- TODO: AlreadyAssumed
  envExtend x (Assumed x t)

-- | Add a definition to the environment. Infers and verifies the definition's
-- type and potentially reconciles it with a previously assumed type for the
-- same name.
define :: Name -> Term -> Pan ()
define x e = do
  logMessage $ "Define" <+> pretty x <+> "= ..."
  t0m <- envLookup x >>= \case
    Nothing                -> return Nothing
    Just (Assumed {_type}) -> return $ Just _type
    Just (Verified {})     -> throwError $ AlreadyDefined x

  logMessage "Prepare typing context Γ"
  g <- envToContext <$> gets environment
  logData g

  logMessage $ "Infer type of" <+> pretty x
  (t1,c1) <- infer g e
  logData t1

  vc <- case t0m of
    Nothing -> pure c1
    Just t0 -> do
      logMessage $ "Require inferred type to be subtype of assumed type"
      t0' <- fresh t0
      c0 <- sub t1 t0'
      return $ c1 ∧ c0
  
  logMessage "Solve VC"
  logData vc
  solve vc >>= \case
    Just s -> do
      logMessage "Apply solution to type"
      let t2 = apply s t1
      logData t2
      envExtend x $ Verified x t0m e t1 vc s t2
    
    Nothing -> do
      throwError $ InvalidVC x vc

-- | Import a module into the environment.
import_ :: Module -> Pan ()
import_ otherModule = do
  logMessage $ "Import" <+> pretty otherModule
  redundant <- elem otherModule <$> gets loadedModules
  unless redundant $ do
    otherSrc <- tryIO NoPV $ Text.readFile $ moduleLocation otherModule
    otherProg <- parseSource (moduleLocation otherModule) otherSrc
    elaborate otherModule otherProg
