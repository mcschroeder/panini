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
  , liftError
  , (??)
  , liftE
  , (?)
  , liftIO
  , tryIO
  , logError
  , logMessage
  , logData
  , logEvent
  , logRegexInfo
  , (§)
  , (§§)
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
import System.Time.Extra

-------------------------------------------------------------------------------

-- | /Panini/ monad.
type Pan e = StateT PanState (ExceptT e IO)

-- | Run an action in the /Panini/ monad with the given starting state. Returns
-- either an unrecoverable error or the result of the computation and the final
-- state (which might contain other kinds of errors and warnings).
runPan :: PanState -> Pan Error a -> IO (Either Error (a, PanState))
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
  , regexTimeout :: Double  -- ^ regex simplifier timeout, in seconds
  
  , debugTraceFrontendGraph :: Bool
  }

defaultState :: PanState
defaultState = PanState
  { environment = mempty
  , kvarCount = 0
  , loadedModules = []
  , eventHandler = const (return ())
  , smtTimeout = 1
  , regexTimeout = 5
  , debugTraceFrontendGraph = False
  }

-------------------------------------------------------------------------------

-- | Throw an `Error` in the /Panini/ monad.
throwError :: e -> Pan e a
throwError err = lift $ throwE err

-- | Catch an `Error` in the /Panini/ monad.
catchError :: Pan e1 a -> (e1 -> Pan e2 a) -> Pan e2 a
catchError m h = StateT $ \s -> runStateT m s `catchE` \e1 -> runStateT (h e1) s

-- | Try an action and return any thrown `Error`.
tryError :: Pan e1 a -> Pan e2 (Either e1 a)
tryError m = catchError (Right <$> m) (return . Left)

-- | Try an action; if an error occurs, log it but don't propagate it further.
continueOnError :: Pan Error () -> Pan Error ()
continueOnError m = catchError m logError

liftError :: (e1 -> e2) -> Pan e1 a -> Pan e2 a
liftError f m = m `catchError` \e1 -> throwError (f e1)

(??) :: Pan e1 a -> (e1 -> e2) -> Pan e2 a
(??) = flip liftError

liftE :: (e1 -> e2) -> Either e1 a -> Pan e2 a
liftE f = either (throwError . f) return

(?) :: Either e1 a -> (e1 -> e2) -> Pan e2 a
(?) = flip liftE

-- TODO: remove PV argument (enrich PV with other functions)
-- | Try an IO action, transforming any 'IOException' that occurs into a Panini
-- 'IOError' with the given provenance.
tryIO :: PV -> IO a -> Pan Error a
tryIO pv m = do
  r <- liftIO $ try @IOException m
  case r of
    Left err -> throwError $ IOError (displayException err) pv
    Right a -> return a

-------------------------------------------------------------------------------

(§) :: HasCallStack => Pretty a => a -> Doc -> Pan e a
x § msg = withFrozenCallStack $ logMessage msg >> logData x >> return x
infix 0 §

(§§) :: HasCallStack => Pretty a => Pan e a -> Doc -> Pan e a
m §§ msg = withFrozenCallStack $ logMessage msg >> m >>= \x -> logData x >> return x
infix 0 §§

logEvent :: Event -> Pan e ()
logEvent d = do
  f <- gets eventHandler
  liftIO $ f d

logError :: Error -> Pan e ()
logError = logEvent . ErrorEvent

logMessage :: HasCallStack => Doc -> Pan e ()
logMessage msg = logEvent $ LogMessage src msg
  where src = getPaniniModuleName callStack

logData :: (HasCallStack, Pretty a) => a -> Pan e ()
logData = logEvent . LogData src . pretty
  where src = getPaniniModuleName callStack

getPaniniModuleName :: CallStack -> String
getPaniniModuleName cs =
  let loc = srcLocModule $ snd $ head $ getCallStack cs
  in fromMaybe loc $ List.stripPrefix "Panini." loc

logRegexInfo :: Pan e ()
logRegexInfo = do
  t <- showDuration <$> gets regexTimeout
  logEvent $ LogMessage "Regex" $ "Simplifier timeout:" <+> pretty t
