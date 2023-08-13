{-# LANGUAGE RecordWildCards #-}
module Panini.Monad
  ( Pan
  , runPan
  , PanState(..)
  , defaultState
  , throwError
  , catchError
  , tryError
  , continueOnError
  , liftIO
  , tryIO
  , logError
  , logMessage
  , logData
  , logEvent
  , getInferredTypes
  , getInferredGrammars
  , panic
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Generics.Uniplate.Operations
import Data.List qualified as List
import Data.Maybe
import GHC.Stack
import Panini.Environment
import Panini.Error
import Panini.Events
import Panini.Modules
import Panini.Pretty
import Panini.Provenance
import Panini.Syntax
import Prelude
import Panini.Abstract.AString
import Data.Map.Strict qualified as Map
import Data.Function

-------------------------------------------------------------------------------

-- | /Panini/ monad.
type Pan = StateT PanState (ExceptT Error IO)

-- TODO: return state as well
runPan :: PanState -> Pan a -> IO (Either Error a)
runPan s0 m = runExceptT $ evalStateT m' s0
  where
    m' = m `catchError` \e -> logError e >> throwError e

-------------------------------------------------------------------------------

data PanState = PanState { 
    environment :: !Environment  -- ^ elaborator environment
  , kvarCount :: !Int  -- ^ source for fresh κ-variable names
  , loadedModules :: ![Module]

  -- | Function for handling diagnostic events. Called synchronously whenever an
  -- event occurs. Default is @const (return ())@.
  , eventHandler :: Event -> IO ()
  }

defaultState :: PanState
defaultState = PanState
  { environment = mempty
  , kvarCount = 0
  , loadedModules = []
  , eventHandler = const (return ())
  }

-------------------------------------------------------------------------------

-- | Throw an `Error` in the /Panini/ monad.
throwError :: Error -> Pan a
throwError err = lift $ throwE err

-- | Catch an `Error` in the /Panini/ monad.
catchError :: Pan a -> (Error -> Pan a) -> Pan a
catchError = liftCatch catchE

-- | Try an action and return any thrown `Error`.
tryError :: Pan a -> Pan (Either Error a)
tryError m = catchError (Right <$> m) (return . Left)

-- | Try an action; if an error occurs, log it but don't propagate it further.
continueOnError :: Pan () -> Pan ()
continueOnError m = catchError m logError

-- TODO: remove PV argument (enrich PV with other functions)
-- | Try an IO action, transforming any 'IOException' that occurs into a Panini
-- 'IOError' with the given provenance.
tryIO :: PV -> IO a -> Pan a
tryIO pv m = do
  r <- liftIO $ try @IOException m
  case r of
    Left err -> throwError $ IOError pv $ displayException err
    Right a -> return a

-------------------------------------------------------------------------------

logEvent :: Event -> Pan ()
logEvent d = do
  f <- gets eventHandler
  liftIO $ f d

logError :: Error -> Pan ()
logError = logEvent . ErrorEvent

logMessage :: HasCallStack => Doc -> Pan ()
logMessage msg = logEvent $ LogMessage src msg
  where src = getPaniniModuleName callStack

logData :: (HasCallStack, Pretty a) => a -> Pan ()
logData = logEvent . LogData src . pretty
  where src = getPaniniModuleName callStack

getPaniniModuleName :: CallStack -> String
getPaniniModuleName cs =
  let loc = srcLocModule $ snd $ head $ getCallStack cs
  in fromMaybe loc $ List.stripPrefix "Panini." loc

-------------------------------------------------------------------------------

-- TODO: these don't really belong here

-- | Return type signatures for all definitions in the environment whose final
-- solved type was differen than the originally assumed (given) one, sorted by
-- order of appearance in source files.
getInferredTypes :: Pan [TypeSig]
getInferredTypes = map (toTypeSig . snd)
                 . List.sortBy (compare `on` getPV . fst) 
                 . Map.toList 
                 . Map.filter hasInferredType 
                 <$> gets environment

-- | Return all inferred grammars in the environment.
getInferredGrammars :: Pan [AString]
getInferredGrammars = concatMap extractGrammars 
                    . map (\(TypeSig _ t) -> t) 
                    <$> getInferredTypes
  where    
    -- TODO: this is pretty hacky and limited
    extractGrammars :: Type -> [AString]
    extractGrammars (TFun _ t1 t2 _) = extractGrammars t1 ++ extractGrammars t2
    extractGrammars t@(TBase x TString (Known p) _) = case p of
      PRel (EVar y :∈: EStrA s) | x == y -> [s]
      _ | not $ null [True | PRel (_ :∈: _) <- universe p ] -> 
          panic $ "extractGrammars: irregular grammar:" <+> pretty t
      _ -> []
    extractGrammars _ = []

-------------------------------------------------------------------------------

panic :: HasCallStack => Doc -> a
panic msg = errorWithoutStackTrace $ 
  "panic! " ++ showPretty msg ++ "\n\n" ++ prettyCallStack callStack
