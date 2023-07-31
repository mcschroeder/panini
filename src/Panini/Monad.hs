module Panini.Monad
  ( Pan
  , runPan
  , PanState(..)
  , defaultState
  , throwError
  , catchError
  , tryError
  , liftIO
  , tryIO
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Text (Text)
import Panini.Environment
import Panini.Error
import Panini.Provenance
import Prelude

-------------------------------------------------------------------------------

-- | /Panini/ monad.
type Pan = StateT PanState (ExceptT Error IO)

runPan :: PanState -> Pan a -> IO (Either Error a)
runPan s0 m = runExceptT $ evalStateT m s0

-------------------------------------------------------------------------------

data PanState = PanState { 
    colorOutput :: !Bool  -- ^ Whether to colorize terminal output.
  
  -- | Whether to use Unicode symbols when pretty printing. Affects both
  -- terminal output and log files.
  , unicodeOutput :: !Bool

  , environment :: !Environment  -- ^ elaborator environment
  , kvarCount :: !Int  -- ^ source for fresh κ-variable names
  , loadedModules :: ![FilePath]

  -- | Function for printing diagnostics to the terminal. If 'Nothing', no
  -- diagnostics are printed (not even errors).
  , logTermPrint :: Maybe (Text -> IO ())
  }

defaultState :: PanState
defaultState = PanState
  { colorOutput = True
  , unicodeOutput = True
  , environment = mempty
  , kvarCount = 0
  , loadedModules = []
  , logTermPrint = Nothing
  }

-------------------------------------------------------------------------------

-- | Throw an `Error` in the /Panini/ monad.
throwError :: Error -> Pan a
throwError = lift . throwE

-- | Catch an `Error` in the /Panini/ monad.
catchError :: Pan a -> (Error -> Pan a) -> Pan a
catchError = liftCatch catchE

-- | Try an action and return any thrown `Error`.
tryError :: Pan a -> Pan (Either Error a)
tryError m = catchError (Right <$> m) (return . Left)

-- | Try an IO action, transforming any 'IOException' that occurs into a Panini
-- 'IOError' with the given provenance.
tryIO :: PV -> IO a -> Pan a
tryIO pv m = do
  r <- liftIO $ try @IOException m
  case r of
    Left err -> throwError $ IOError pv $ displayException err
    Right a -> return a
