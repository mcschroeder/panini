{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}

module Panini.Language.Elaborator 
  ( elaborateProgram
  , elaborateStatement
  , envToContext -- TODO: weird place for this?
  ) where

import Control.Monad.Extra
import Control.Monad.Trans.State.Strict
import Data.Map qualified as Map
import Data.Maybe
import Data.Text.IO qualified as Text
import Panini.Error
import Panini.Language.AST
import Panini.Language.Environment
import Panini.Language.Infer
import Panini.Logger
import Panini.Logic.Solver
import Panini.Monad
import Panini.Names
import Panini.Parser
import Panini.Pretty.Printer
import Prelude
import System.FilePath
import Panini.Logic.Solver.Assignment

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

tryElab :: Pan a -> Pan a
tryElab m = do
    state0 <- get
    tryError m >>= \case
      Right a -> return a
      Left  e -> put state0 >> throwError e

-- | Elaborate a program by elaborating all of its statments and updating the
-- environment accordingly. If elaboration fails at any point, an error is
-- thrown and any intermittent changes to the environment are rolled back.
elaborateProgram :: FilePath -> Program -> Pan ()
elaborateProgram thisModule prog = tryElab $ do
  whenM (elem thisModule <$> gets loadedModules) $ do
    error "elaborateProgram: module reloading not yet implemented" -- TODO
  mapM_ (elaborateStatement thisModule) prog
  modify' (\s -> s { loadedModules = thisModule : s.loadedModules } )

-- | Elaborate a statement and add the resulting definition(s) to the
-- environment. If elaboration fails, an error will be thrown and the
-- environment will remain unchanged.
elaborateStatement :: FilePath -> Statement -> Pan ()
elaborateStatement thisModule stmt = tryElab $ case stmt of
  Assume x t -> do
    logMessageDoc "Elab" $ "Assume" <+> pretty x
    whenJustM (envLookup x) $ \_ -> throwError $ AlreadyDefined x -- TODO: AlreadyAssumed
    envExtend x (Assumed x t)
    logData "Assumed Type" t
  
  Define x t0 e -> do
    logMessageDoc "Elab" $ "Define" <+> pretty x
    logData "Definition" stmt
    whenJustM (envLookup x) $ \_ -> throwError $ AlreadyDefined x
    logMessage "Infer" "Infer type"
    g <- envToContext <$> gets environment
    let g' = Map.insert x t0 g
    logData "Typing Context Γ" g'
    (t1, vc) <- infer g' e
    logData "Inferred Type" t1
    logData "Verification Condition" vc
    s <- solve vc
    let t2 = apply s t1
    envExtend x (Verified x t0 e t1 vc s t2)
    logData "Solution" s
    logData "Solved Typed" t2
  
  Import otherModule0 pv -> do
    let otherModule = takeDirectory thisModule </> otherModule0
    logMessageDoc "Elab" $ "Import" <+> pretty otherModule
    redundant <- elem otherModule <$> gets loadedModules
    unless redundant $ do
      otherSrc <- tryIO pv $ Text.readFile otherModule
      otherProg <- parseSource otherModule otherSrc
      elaborateProgram otherModule otherProg
