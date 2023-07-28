{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}

module Panini.Language.Elaborator where

import Control.Monad
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

-- | Remove a definition from the environment.
envDelete :: Name -> Pan ()
envDelete x = modify' $ \s -> s { environment = Map.delete x s.environment }

-- | Convert an elaborator environment to a typechecking context by throwing
-- away all non-final definitions.
envToContext :: Environment -> Context 
envToContext = Map.map go
  where
    go (Assumed {_givenType}) = _givenType
    go (Verified {_solvedType}) = _solvedType

-------------------------------------------------------------------------------

-- TODO: if elaboration fails, defs should not be added to env

-- | Elaborate all statements in a program (see 'elaborateStatement').
elaborateProgram :: FilePath -> Program -> Pan ()
elaborateProgram =  mapM_ . elaborateStatement

-- | Elaborate a statement and add the corresponding definition(s) to the
-- environment.
elaborateStatement :: FilePath -> Statement -> Pan ()
elaborateStatement thisModule = \case
  Assume x t -> do
    logMessageDoc "Elab" $ "Assume" <+> pretty x
    envLookup x >>= \case
      Just _ -> throwError $ AlreadyDefined x -- TODO: AlreadyAssumed
      Nothing -> do
        envExtend x (Assumed x t)
        logData "Assumed Type" t
  
  stmt@(Define x t0 e) -> do
    logMessageDoc "Elab" $ "Define" <+> pretty x
    logData "Definition" stmt
    envLookup x >>= \case
      Just _ -> throwError $ AlreadyDefined x
      Nothing -> do
        logMessage "Infer" "Infer type"
        g <- envToContext <$> gets environment
        let g' = Map.insert x t0 g
        logData "Typing Context Γ" g'
        (t,vc) <- infer g' e
        logData "Inferred Type" t
        logData "Verification Condition" vc
        s <- solve vc
        envExtend x (Verified x t0 e t vc s (apply s t))
  
  Import otherModule0 pv -> do
    let otherModule = takeDirectory thisModule </> otherModule0
    logMessageDoc "Elab" $ "Import" <+> pretty otherModule
    redundant <- elem otherModule <$> gets loadedModules
    unless redundant $ do
      otherSrc <- tryIO pv $ Text.readFile otherModule
      otherProg <- parseSource otherModule otherSrc
      elaborateProgram otherModule otherProg
      modify' (\s -> s{loadedModules = otherModule:s.loadedModules})
