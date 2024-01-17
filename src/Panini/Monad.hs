{-# LANGUAGE RecordWildCards #-}
module Panini.Monad
  ( Pan
  , runPan
  , PanState(..)
  , defaultState
  , gets
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
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Function
import Data.List qualified as List
import Data.Maybe
import GHC.Stack
import Panini.Environment
import Panini.Error
import Panini.Events
import Panini.Modules
import Panini.Pretty
import Panini.Provenance
import Prelude

-------------------------------------------------------------------------------

-- | /Panini/ monad.
type Pan = StateT PanState (ExceptT Error IO)

-- | Run an action in the /Panini/ monad with the given starting state. Returns
-- either an unrecoverable error or the result of the computation and the final
-- state (which might contain other kinds of errors and warnings).
runPan :: PanState -> Pan a -> IO (Either Error (a, PanState))
runPan s0 m = runExceptT $ runStateT m' s0
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

  , smtTimeout :: Int  -- ^ SMT solver timeout, in seconds
  }

defaultState :: PanState
defaultState = PanState
  { environment = mempty
  , kvarCount = 0
  , loadedModules = []
  , eventHandler = const (return ())
  , smtTimeout = 10
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
