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
  , logMessageDoc
  , logData
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Text (Text)
import Panini.Diagnostics
import Panini.Environment
import Panini.Error
import Panini.Pretty.Printer
import Panini.Provenance
import Prelude

-------------------------------------------------------------------------------

-- | /Panini/ monad.
type Pan = StateT PanState (ExceptT Error IO)

-- TODO: return state as well
-- TODO: log final error
runPan :: PanState -> Pan a -> IO (Either Error a)
runPan s0 m = runExceptT $ evalStateT m s0

-------------------------------------------------------------------------------

data PanState = PanState { 
    environment :: !Environment  -- ^ elaborator environment
  , kvarCount :: !Int  -- ^ source for fresh κ-variable names
  , loadedModules :: ![FilePath]

  -- | Function for printing diagnostics to the terminal. If 'Nothing', no
  -- diagnostics are printed (not even errors).
  , logTermPrint :: Maybe (Text -> IO ())

  -- | Function for logging diagnostic and debugging information. Called
  -- synchronously. Default is @const (return ())@.
  , logDiagnostic :: Diagnostic -> IO ()
  }

defaultState :: PanState
defaultState = PanState
  { environment = mempty
  , kvarCount = 0
  , loadedModules = []
  , logTermPrint = Nothing
  , logDiagnostic = const (return ())
  }

-------------------------------------------------------------------------------

-- TODO: automatically log error
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

-- | Try an IO action, transforming any 'IOException' that occurs into a Panini
-- 'IOError' with the given provenance.
tryIO :: PV -> IO a -> Pan a
tryIO pv m = do
  r <- liftIO $ try @IOException m
  case r of
    Left err -> throwError $ IOError pv $ displayException err
    Right a -> return a

-------------------------------------------------------------------------------

logD :: Diagnostic -> Pan ()
logD d = do
  f <- gets logDiagnostic
  liftIO $ f d

logError :: Error -> Pan ()
logError = logD . DiagError

logMessage :: String -> String -> Pan ()
logMessage src = logMessageDoc src . pretty

logMessageDoc :: String -> Doc -> Pan ()
logMessageDoc src msg = logD $ DiagMessage src msg

logData :: Pretty a => String -> a -> Pan ()
logData label a = logD $ Data label (pretty a)
