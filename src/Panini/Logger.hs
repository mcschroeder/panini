module Panini.Logger
  ( logOutput
  , logMessage
  , logMessageDoc
  , logData
  ) where

import Data.Text.IO qualified as Text
import Panini.Pretty.Printer
import Prelude
import System.Console.ANSI
import Control.Monad.IO.Class
import Control.Monad
import Panini.Monad
import Control.Monad.Trans.State.Strict
import Data.Maybe

-------------------------------------------------------------------------------

-- TODO: log to file
-- TODO: use getExternalPrint for REPL output

getTermRenderOptions :: PanState -> IO RenderOptions
getTermRenderOptions s = do
  w <- fmap snd <$> getTerminalSize
  return RenderOptions
    { styling = 
        if s.colorOutput 
          then Just defaultStyling
          else Nothing 
    , unicode = s.unicodeOutput
    , fixedWidth = w
    }

logOutput :: Pretty a => a -> Pan ()
logOutput a = do
  s <- get
  liftIO $ do
    opts <- getTermRenderOptions s
    Text.putStrLn $ renderDoc opts $ pretty a

logMessage :: String -> String -> Pan ()
logMessage src = logMessageDoc src . pretty

logMessageDoc :: String -> Doc -> Pan ()
logMessageDoc src msg = do
  s <- get
  when s.debugMode $ liftIO $ do
    opts <- getTermRenderOptions s
    Text.putStrLn $ renderDoc opts $ 
      marginalia (pretty src <+> symDivDiag) <+> aMessage msg

logData :: Pretty a => String -> a -> Pan ()
logData label a = do
  s <- get
  when s.debugMode $ liftIO $ do
    opts <- getTermRenderOptions s
    let w = fromMaybe 80 opts.fixedWidth
    let divider = mconcat $ replicate (w - length label - 1) symDivH
    Text.putStrLn $ renderDoc opts $ 
      marginalia (pretty divider <+> pretty label)
      <\\> pretty a 
      <> "\n"

-- TODO:
-- logWarning :: Pretty a => a -> Pan ()
-- logError :: Error -> Pan ()