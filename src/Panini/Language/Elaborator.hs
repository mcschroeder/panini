{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}

module Panini.Language.Elaborator where

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
import Panini.Modules
import Panini.Monad
import Panini.Names
import Panini.Parser
import Panini.Pretty.Printer
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

-- | Elaborate all statements in a program (see 'elaborateStatement').
elaborateProgram :: Module -> Program -> Pan ()
elaborateProgram =  mapM_ . elaborateStatement

-- | Elaborate a statement and add the corresponding definition(s) to the
-- environment. Most errors that occur along the way (e.g., during type
-- inference or SMT solving) will still result in the construction of a valid
-- 'Definition' (e.g., 'Rejected') and so are not re-thrown but merely logged.
-- In case an error prohibits the construction of a 'Definition' it is re-thrown
-- and the elaboration aborts.
elaborateStatement :: Module -> Statement -> Pan ()
elaborateStatement curMod = \case
  Assume x t -> do
    logMessageDoc "Elab" $ "Assume" <+> pretty x
    def0 <- envLookup x
    case def0 of
      Just (Assumed _ t0) | t0 == t -> logData "Previously Assumed Type" t
      Just _ -> throwError $ AlreadyDefined x -- TODO: AlreadyAssumed
      Nothing -> do
        envExtend x (Assumed x t)
        logData "Assumed Type" t
  
  stmt@(Define x t0 e) -> do
    logMessageDoc "Elab" $ "Define" <+> pretty x
    logData "Definition" stmt
    def0 <- envLookup x
    case def0 of
      Just _ -> throwError $ AlreadyDefined x
      Nothing -> do
        logMessage "Infer" "Infer type"
        g <- envToContext <$> gets environment
        let g' = Map.insert x t0 g
        logData "Typing Context Γ" g'
        r1 <- tryError $ infer g' e
        case r1 of
          Left err -> do
            envExtend x (Rejected x t0 e err)
            logError err
          Right (t,vc) -> do
            envExtend x (Inferred x t0 e t vc)
            logData "Inferred Type" t
            logData "Verification Condition" vc
            r2 <- tryError $ solve vc
            case r2 of
              Left err -> do
                envExtend x (Invalid x t0 e t vc err)
                logError err
              Right s -> do
                envExtend x (Verified x t0 e t vc s)
  
  -- TODO: don't trace log through imports, maybe?
  Import impPath pv -> do
    -- TODO: check if module already loaded!
    impMod <- tryIO pv $ findModule impPath (Just curMod) []
    case impMod of
      Nothing -> undefined -- TODO
      Just impMod' -> do
        let impPath' = fromJust $ modulePath impMod'
        logMessageDoc "Elab" $ "Import" <+> pretty impMod'
        impSrc <- tryIO pv $ Text.readFile impPath'
        case parseProgram impPath' impSrc of
          Left  err  -> throwError err
          Right prog -> elaborateProgram impMod' prog
