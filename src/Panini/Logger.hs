module Panini.Logger
  ( Logger
  , newLogger
  , LogLevel(..)
  , setLogLevel
  , LogSource
  , HasLogger(..)
  , logBegin
  , logEnd
  , logMessage
  , logData
  ) where

import Data.Text.IO qualified as Text
import Panini.Pretty.Printer
import Prelude
import System.Console.ANSI
import Data.Maybe
import Formatting
import Control.Monad.IO.Class
import Control.Monad

-------------------------------------------------------------------------------

data Logger = Logger
  { level  :: LogLevel
  }

newLogger :: Logger
newLogger = Logger { level = Trace }

setLogLevel :: LogLevel -> Logger -> Logger
setLogLevel l logger = logger { level = l }

data LogLevel = Info | Debug | Trace
  deriving stock (Eq, Ord, Show)

type LogSource = String

class HasLogger m where
  getLogger :: m Logger

logBegin :: (HasLogger m, MonadIO m) => String -> m ()
logBegin _ = do
  Logger{} <- getLogger
  liftIO $ do
    w <- getTermWidth
    putStr $ "╭" ++ replicate 8 '─' ++ "─┬─" ++ replicate (w - 8 - 5) '─' ++ "╮"

logEnd :: (HasLogger m, MonadIO m) => m ()
logEnd = do
  Logger{} <- getLogger
  liftIO $ do
    w <- getTermWidth
    putStrLn $ "╰" ++ replicate 8 '─' ++ "─┴─" ++ replicate (w - 8 - 5) '─' ++ "╯"

logMessage :: (HasLogger m, MonadIO m) => LogLevel -> LogSource -> String -> m ()
logMessage l s m = do
  logger <- getLogger
  unless (logger.level < l) $ liftIO $ do
    fprint ("│" % lpadded 8 ' ' string % " │ " % string) s m
    w <- getTermWidth
    setCursorColumn (w - 1)
    putStrLn "│"

logData :: (HasLogger m, MonadIO m, Pretty a) => LogLevel -> a -> m ()  
logData l a = do
  logger <- getLogger
  unless (logger.level < l) $ liftIO $ do
    w <- getTermWidth
    let opts = RenderOptions 
          { styling = Just defaultStyling
          , unicode = True
          , fixedWidth = Just w
          }
    putStrLn $ "╰" ++ replicate 8 '─' ++ "─┴─" ++ replicate (w - 8 - 5) '─' ++ "╯"
    Text.putStrLn $ renderDoc opts $ pretty a
    putStrLn $ "╭" ++ replicate 8 '─' ++ "─┬─" ++ replicate (w - 8 - 5) '─' ++ "╮"

getTermWidth :: IO Int
getTermWidth = fromMaybe 80 <$> fmap snd <$> getTerminalSize
