{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}

module Panini.Language.Elaborator where

import Control.Monad.Trans.State.Strict
import Data.Map qualified as Map
import Data.Text.IO qualified as Text
import Panini.Error
import Panini.Language.AST
import Panini.Language.Elaborator.Environment
import Panini.Language.Infer
import Panini.Logger
import Panini.Logic.Solver
import Panini.Monad
import Panini.Names
import Panini.Parser
import Panini.Pretty.Printer
import Prelude
import System.Directory
import System.FilePath

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
envToContext = Map.mapMaybe go
  where
    go (Assumed   {_givenType})    = Just _givenType
    go (Verified  {_inferredType}) = Just _inferredType
    go _                           = Nothing

-------------------------------------------------------------------------------

elaborateProgram :: FilePath -> Program -> Pan ()
elaborateProgram = mapM_ . elaborateStatement

elaborateStatement :: FilePath -> Statement -> Pan ()
elaborateStatement modulePath = \case
  Assume x t -> do
    logMessageDoc "Elab" $ "Assume" <+> pretty x
    def0 <- envLookup x
    case def0 of
      Just (Assumed _ t0) | t0 == t -> logData "Previously Assumed Type" t
      Just _ -> throwError $ AlreadyDefined x  -- TODO: more info
      Nothing -> do
        envExtend x (Assumed x t)
        logData "Assumed Type" t
  
  stmt@(Define x t0 e) -> do
    logMessageDoc "Elab" $ "Define" <+> pretty x
    logData "Definition" stmt
    def0 <- envLookup x
    case def0 of
      Just _  -> throwError $ AlreadyDefined x
      Nothing -> do
        g <- envToContext <$> gets environment
        let g' = Map.insert x t0 g
        logMessage "Infer" "Infer type"
        logData "Typing Context Γ" g'
        r1 <- tryError $ infer g' e
        case r1 of
          Left err -> do
            envExtend x (Rejected x t0 e err)
            throwError err -- TODO: just logError instead of re-throwing ?
          Right (_e',t,vc) -> do
            envExtend x (Inferred x t0 e t vc)
            logData "Inferred Type" t
            logData "Verification Condition" vc
            r <- solve vc
            case r of
              Just s -> do
                envExtend x (Verified x t0 e t vc s)
                logOutput s  -- TODO
              Nothing -> do
                envExtend x (Invalid x t0 e t vc Nothing)
                -- TODO: logError?
  
  Import m -> do
    absModulePath <- liftIO $ makeAbsolute modulePath  -- TODO: catch error
    let importPath = takeDirectory absModulePath </> m
    src <- liftIO $ Text.readFile importPath  -- TODO: handle error
    case parseProgram m src of
      Left  err  -> throwError err
      Right prog -> elaborateProgram m prog
