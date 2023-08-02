{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}

module Panini.Elab
  ( elaborate
  , assume
  , define
  , import_
  , envToContext -- TODO: weird place for this?
  ) where

import Control.Monad.Extra
import Control.Monad.Trans.State.Strict
import Data.Map qualified as Map
import Data.Maybe
import Data.Text.IO qualified as Text
import Panini.Environment
import Panini.Error
import Panini.Infer
import Panini.Logic
import Panini.Monad
import Panini.Parser
import Panini.Pretty.Printer
import Panini.Syntax
import Prelude
import System.FilePath
import Algebra.Lattice
import Panini.Provenance

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

-- TODO: fix module/import mess

-- | Elaborate a program by elaborating all of its statments and updating the
-- environment accordingly. If elaboration fails at any point, an error is
-- thrown and any intermittent changes to the environment are rolled back.
elaborate :: FilePath -> Program -> Pan ()
elaborate thisModule prog = do
  state0 <- get
  tryError (mapM_ elab prog) >>= \case
    Right () -> do
      -- TODO: avoid hardcoded special <repl> module
      unless (thisModule == "<repl>") $
        modify' $ \s -> s { loadedModules = thisModule : s.loadedModules }
    Left err -> do
      put state0
      throwError err
 where
  elab = \case
    Assume x t -> assume x t
    Define x t0 e -> define x t0 e
    Import fp _ -> do
      let otherModule = takeDirectory thisModule </> fp
      import_ otherModule  -- TODO: add provenance to error

-- | Add an assumed type to the environment.
assume :: Name -> Type -> Pan ()
assume x t = do
  logMessageDoc "Elab" $ "Assume" <+> pretty x
  whenJustM (envLookup x) $ \_ -> throwError $ AlreadyDefined x -- TODO: AlreadyAssumed
  envExtend x (Assumed x t)
  logData "Assumed Type" t  

-- | Add a definition to the environment.
define :: Name -> Type -> Term -> Pan ()
define x t0 e = do
  logMessageDoc "Elab" $ "Define" <+> pretty x
  --logData "Definition" stmt
  whenJustM (envLookup x) $ \_ -> throwError $ AlreadyDefined x
  logMessage "Infer" "Infer type"
  g <- envToContext <$> gets environment
  let g' = Map.insert x t0 g
  logData "Typing Context Γ" g'
  (t1, c1) <- infer g' e
  logData "Inferred Type" t1
  c0 <- sub t1 t0
  let vc = c1 ∧ c0
  logData "Verification Condition" vc
  s <- solve vc
  logData "Solution" s
  let t2 = apply s t1
  logData "Solved Typed" t2
  envExtend x $ Verified x t0 e t1 vc s t2

-- | Import a module into the environment.
import_ :: FilePath -> Pan ()
import_ otherModule = do
  logMessageDoc "Elab" $ "Import" <+> pretty otherModule
  redundant <- elem otherModule <$> gets loadedModules
  unless redundant $ do
    otherSrc <- tryIO NoPV $ Text.readFile otherModule
    otherProg <- parseSource otherModule otherSrc
    elaborate otherModule otherProg
