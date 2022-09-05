module Panini.Monad
  ( Pan
  , throwError
  , catchError
  , tryError
  , PanState(..)
  , initState
  , liftIO  
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Panini.Elaborator.Environment
import Panini.Error
import Panini.Logger
import Prelude

-------------------------------------------------------------------------------

-- | /Panini/ monad.
type Pan = StateT PanState (ExceptT Error IO)

-- | Throw an `Error` in the /Panini/ monad.
throwError :: Error -> Pan a
throwError = lift . throwE

-- | Catch an `Error` in the /Panini/ monad.
catchError :: Pan a -> (Error -> Pan a) -> Pan a
catchError = liftCatch catchE

-- | Try an action and return any thrown `Error`.
tryError :: Pan a -> Pan (Either Error a)
tryError m = catchError (Right <$> m) (return . Left)

data PanState = PanState
  { environment :: !Environment  -- ^ elaborator environment
  , logger      :: !Logger
  , kvarCount   :: !Int          -- ^ source for fresh κ-variable names
  }

initState :: PanState
initState = PanState 
  { environment = mempty
  , logger = newLogger
  , kvarCount = 0
  }

instance HasLogger Pan where
  getLogger = gets logger
