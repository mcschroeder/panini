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
import Panini.Pretty.Printer
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
    go (Assumed {_givenType}) = _givenType
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
      error "reloading modules is not yet implemented" -- TODO
  tryError (mapM_ elab prog) >>= \case
    Right () -> do
      unless (thisModule == replModule) $
        modify' $ \s -> s { loadedModules = thisModule : s.loadedModules }
    Left err -> do
      put env0
      throwError err
 where
  elab = \case
    Assume x t    -> assume x t
    Define x t0 e -> define x t0 e
    Import fp _   -> import_ $ moduleRelativeTo thisModule fp
                      -- TODO: add provenance to error

-- | Add an assumed type to the environment.
assume :: Name -> Type -> Pan ()
assume x t = do
  logMessage $ "Assume" <+> pretty x <+> symColon <+> pretty t
  whenJustM (envLookup x) $ \_ -> throwError $ AlreadyDefined x -- TODO: AlreadyAssumed
  envExtend x (Assumed x t)

-- | Add a definition to the environment.
define :: Name -> Type -> Term -> Pan ()
define x t0 e = do
  logMessage $ "Define" <+> pretty x
  --logData "Definition" stmt
  whenJustM (envLookup x) $ \_ -> throwError $ AlreadyDefined x
  logMessage "Prepare typing context Γ"
  g <- envToContext <$> gets environment
  let g' = Map.insert x t0 g
  logData g'
  logMessage $ "Infer type of" <+> pretty x <+> "in Γ"
  (t1, c1) <- infer g' e
  t̂₀ <- fresh t0
  c0 <- sub t1 t̂₀
  let vc = c1 ∧ c0
  logData $ pretty t1 <\\> "⫤" <\\> pretty c1
  logMessage "Solve verification condition"
  s <- solve vc
  logMessage "Apply solution to type"
  let t2 = apply s t1
  logData t2
  envExtend x $ Verified x t0 e t1 vc s t2

-- | Import a module into the environment.
import_ :: Module -> Pan ()
import_ otherModule = do
  logMessage $ "Import" <+> pretty otherModule
  redundant <- elem otherModule <$> gets loadedModules
  unless redundant $ do
    otherSrc <- tryIO NoPV $ Text.readFile $ moduleLocation otherModule
    otherProg <- parseSource (moduleLocation otherModule) otherSrc
    elaborate otherModule otherProg
