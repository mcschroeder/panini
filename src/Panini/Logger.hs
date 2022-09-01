module Panini.Logger where

import Data.Text.IO qualified as Text
import Panini.Pretty.Printer
import Prelude
import System.Console.ANSI
import Data.Maybe
import System.Clock
import Control.Exception (evaluate)
import Formatting
import Formatting.Clock
import Control.Monad.IO.Class
import Control.Monad

-------------------------------------------------------------------------------

-- TODO: timestamps
-- TODO: log complete trace to file

data Logger = Logger
  { logLevel  :: LogLevel
--  , logDir    :: Maybe FilePath
--  , logHandle :: IORef (Maybe Handle)
  }

data LogLevel = Info | Debug | Trace
  deriving stock (Eq, Ord, Show)

type LogSource = String

class HasLogger m where
  getLogger :: m Logger

logBegin :: (HasLogger m, MonadIO m) => String -> m ()
logBegin _ = do
  Logger{} <- getLogger
  w <- getTermWidth
  liftIO $ putStrLn $ replicate 8 '═' ++ "═╤═" ++ replicate (w - 8 - 3) '═'

logEnd :: (HasLogger m, MonadIO m) => m ()
logEnd = do
  Logger{} <- getLogger
  w <- getTermWidth
  liftIO $ putStrLn $ replicate 8 '═' ++ "═╧═" ++ replicate (w - 8 - 3) '═'

logMessage :: (HasLogger m, MonadIO m) => LogLevel -> LogSource -> String -> m ()
logMessage l s m = do
  Logger{logLevel} <- getLogger
  unless (logLevel < l) $
    liftIO $ fprintLn (lpadded 8 ' ' string % " │ " % string) s m

logData :: (HasLogger m, MonadIO m, Pretty a) => LogLevel -> a -> m ()  
logData l a = do
  Logger{logLevel} <- getLogger
  unless (logLevel < l) $ liftIO $ do
    w <- getTermWidth
    let opts = RenderOptions 
          { styling = Just defaultStyling
          , unicode = True
          , fixedWidth = Just w
          }
    putStrLn $ replicate 8 '─' ++ "─┴─" ++ replicate (w - 8 - 3) '─'
    Text.putStrLn $ renderDoc opts $ pretty a
    putStrLn $ replicate 8 '─' ++ "─┬─" ++ replicate (w - 8 - 3) '─'

logTime :: (HasLogger m, MonadIO m) => LogLevel -> LogSource -> String -> a -> m a
logTime l s m a = logTimeM l s m (liftIO $ evaluate a)

logTimeM :: (HasLogger m, MonadIO m) => LogLevel -> LogSource -> String -> m a -> m a  
logTimeM l s m a = do
  Logger{logLevel} <- getLogger
  if logLevel < l
    then a
    else do
      start <- liftIO $ do
        fprint (lpadded 8 ' ' string % " │ " % string) s m
        getTime Monotonic    
      b <- a
      liftIO $ do
        end <- getTime Monotonic
        w <- getTermWidth
        let x = fromIntegral $ w - 8 - 3 - (length m)
        let t = format timeSpecs start end
        fprintLn (lpadded x ' ' text) t
      return b
  
getTermWidth :: MonadIO m => m Int
getTermWidth = liftIO $ fromMaybe 80 <$> fmap snd <$> getTerminalSize
