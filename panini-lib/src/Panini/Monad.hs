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
  , liftE
  , (?)
  , liftError
  , (??)
  , liftIO
  , tryIO
  , report
  , warn
  , info
  , trace
  , (§)
  , (§§)
  , logRegexInfo
  , logMessage
  , logData
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
import Panini.Diagnostic
import Panini.Elab.Environment
import Panini.Elab.Module
import Panini.Pretty
import Panini.Provenance
import Prelude
import System.Time.Extra

-------------------------------------------------------------------------------

-- | The Panini monad, parameterized by the type of errors it produces.
type Pan e = StateT PanState (ExceptT e IO)

-- | Run an action in the Panini monad with the given starting state. Returns
-- either an unrecoverable error or the result of the computation and the final
-- state (which might contain other kinds of errors and warnings).
runPan 
  :: (Diagnostic e, HasProvenance e) 
  => PanState -> Pan e a -> IO (Either e (a, PanState))
runPan s0 m = runExceptT $ runStateT m' s0
  where
    m' = m `catchError` \e -> report SevError (getPV e) e >> throwError e

-------------------------------------------------------------------------------

data PanState = PanState { 
    environment :: !Environment  -- ^ elaborator environment
  , kvarCount :: !Int  -- ^ source for fresh κ-variable names
  , loadedModules :: ![Module]

  -- | Function for handling diagnostic events. Called synchronously whenever an
  -- event occurs. Default is @const (return ())@.
  , diagnosticHandler :: forall a. Diagnostic a => DiagnosticEnvelope a -> IO ()

  , smtTimeout :: Int  -- ^ SMT solver timeout, in seconds
  , regexTimeout :: Double  -- ^ regex simplifier timeout, in seconds
  
  , debugTraceFrontendGraph :: Bool
  }

defaultState :: PanState
defaultState = PanState
  { environment = mempty
  , kvarCount = 0
  , loadedModules = []
  , diagnosticHandler = const (return ())
  , smtTimeout = 1
  , regexTimeout = 5
  , debugTraceFrontendGraph = False
  }

-------------------------------------------------------------------------------

-- | Throw an error in the Panini monad, halting the current computation.
throwError :: e -> Pan e a
throwError e = lift (throwE e)

-- | Catch an error in the Panini monad.
catchError :: Pan e1 a -> (e1 -> Pan e2 a) -> Pan e2 a
catchError m h = StateT $ \s -> runStateT m s `catchE` \e -> runStateT (h e) s

-- | Try an action and return any thrown error event.
tryError :: Pan e1 a -> Pan e2 (Either e1 a)
tryError m = catchError (Right <$> m) (return . Left)

-- | Try an action; if an error occurs, report it but don't propagate it further.
continueOnError :: (Diagnostic e1, HasProvenance e1) => Pan e1 () -> Pan e2 ()
continueOnError m = m `catchError` \e -> report SevError (getPV e) e

-- | Lift an 'Either' into the Panini monad, treating 'Left' as an error.
liftE :: (e1 -> e2) -> Either e1 a -> Pan e2 a
liftE f = either (throwError . f) return

-- | An infix synonym for 'liftE'.
(?) :: Either e1 a -> (e1 -> e2) -> Pan e2 a
(?) = flip liftE

-- | Lift a Panini computation with one type of error into a Panini computation
-- with a different type of error.
liftError :: (e1 -> e2) -> Pan e1 a -> Pan e2 a
liftError f m1 = m1 `catchError` \e1 -> throwError (f e1)

-- | An infix synonym for 'liftEvents'.
(??) :: Pan e1 a -> (e1 -> e2) -> Pan e2 a
(??) = flip liftError

-------------------------------------------------------------------------------

-- | Try an IO action, transforming any 'IOError' into a Panini diagnostic.
tryIO :: IO a -> Pan IOError a
tryIO m = either throwError return =<< liftIO (try @IOError m)

-------------------------------------------------------------------------------

report :: (HasCallStack, Diagnostic a) => Severity -> PV -> a -> Pan e ()
report severity provenance diagnostic = do
  let rapporteur = getPaniniModuleName callStack
  PanState{diagnosticHandler} <- get
  liftIO $ diagnosticHandler DiagnosticEnvelope{..}

getPaniniModuleName :: CallStack -> String
getPaniniModuleName cs =
  let loc = srcLocModule $ snd $ head $ getCallStack cs
  in fromMaybe loc $ List.stripPrefix "Panini." loc

warn :: (HasCallStack, Diagnostic e, HasProvenance e) => e -> Pan e ()
warn w = withFrozenCallStack $ report SevWarning (getPV w) w
-- TODO: if treatWarningsAsErrors then throwError w else ...

info :: (HasCallStack, Diagnostic a) => a -> Pan e ()
info = withFrozenCallStack $ report SevInfo NoPV

trace :: (HasCallStack, Diagnostic a) => a -> Pan e ()
trace = withFrozenCallStack $ report SevTrace NoPV

(§) :: (HasCallStack, Pretty a) => a -> Doc -> Pan e a
x § msg = withFrozenCallStack $ do 
  info msg
  trace (pretty x)
  return x
infix 0 §

(§§) :: (HasCallStack, Pretty a) => Pan e a -> Doc -> Pan e a
m §§ msg = withFrozenCallStack $ do
  info msg
  x <- m
  trace (pretty x)
  return x
infix 0 §§

-- TODO: move somewhere else
logRegexInfo :: Pan e ()
logRegexInfo = do
  t <- showDuration <$> gets regexTimeout
  info $ "Simplifier timeout:" <+> pretty t

-- TODO: get rid of this synonym
logMessage :: HasCallStack => Doc -> Pan e ()
logMessage = withFrozenCallStack info

-- TODO: get rid of this synonym
logData :: (HasCallStack, Pretty a) => a -> Pan e ()
logData = withFrozenCallStack $ trace . pretty 
