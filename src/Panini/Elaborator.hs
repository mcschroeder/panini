{-# LANGUAGE RecordWildCards #-}

module Panini.Elaborator where

import Control.Monad.Trans.State.Strict
import Data.Map qualified as Map
import Data.Text.IO qualified as Text
import Panini.Elaborator.Environment
import Panini.Error
import Panini.Infer
import Panini.Logger
import Panini.Monad
import Panini.Parser
import Panini.Pretty.Printer
import Panini.Solver
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

-- | Retrieve a definition from the environment.
envLookup :: Name -> Pan (Maybe Definition)
envLookup x = Map.lookup x <$> gets environment

-- | Extend the environment with a new definition.
envExtend :: Name -> Definition -> Pan ()
envExtend x d = modify' $ \s -> s { environment = Map.insert x d s.environment }

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

elaborateProgram :: Program -> Pan ()
elaborateProgram = mapM_ elaborateStatement

elaborateStatement :: Statement -> Pan ()
elaborateStatement = \case
  Assume x t -> do
    logMessage Debug "Elab" $ "Assume " ++ showPretty x
    logData Trace t
    def0 <- envLookup x
    case def0 of
      Just _  -> throwError $ AlreadyDefined x
      Nothing -> envExtend x (Assumed x t)    
  
  stmt@(Define x t0 e) -> do
    logMessage Info "Elab" $ "Define " ++ showPretty x
    logData Trace stmt
    def0 <- envLookup x
    case def0 of
      Just _  -> throwError $ AlreadyDefined x
      Nothing -> do
        g <- envToContext <$> gets environment
        let g' = Map.insert x t0 g
        logMessage Info "Infer" "Infer type"
        logData Trace g'
        r1 <- tryError $ infer g' e
        case r1 of
          Left err -> do
            envExtend x (Rejected x t0 e err)
            throwError err -- ?
          Right (_e',t,vc) -> do
            envExtend x (Inferred x t0 e t vc)
            logData Trace t
            logData Trace vc
            r <- solve vc
            case r of
              Just s -> envExtend x (Verified x t0 e t vc s)
              Nothing -> envExtend x (Invalid x t0 e t vc Nothing)
  
  Import m -> do
    src <- liftIO $ Text.readFile m  -- TODO: handle error
    case parseProgram m src of
      Left  err  -> throwError err
      Right prog -> elaborateProgram prog
