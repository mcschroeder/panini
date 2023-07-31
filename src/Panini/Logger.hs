module Panini.Logger
  ( logMessage
  , logMessageDoc
  , logData
  , logError
  ) where

import Panini.Pretty.Printer
import Prelude
import System.Console.ANSI
import Control.Monad.IO.Class
import Panini.Monad
import Control.Monad.Trans.State.Strict
import Panini.Error
import Control.Monad.Extra

-------------------------------------------------------------------------------

-- TODO: log to file

putTerm :: Doc -> Pan ()
putTerm doc = do
  env <- get
  whenJust env.logTermPrint $ \termPrint -> liftIO $ do
    w <- fmap snd <$> getTerminalSize
    let opts = RenderOptions 
          { styling = pureIf env.colorOutput defaultStyling
          , unicode = env.unicodeOutput
          , fixedWidth = w
          }
    termPrint $ renderDoc opts doc

logMessage :: String -> String -> Pan ()
logMessage src = logMessageDoc src . pretty

logMessageDoc :: String -> Doc -> Pan ()
logMessageDoc src msg =
  putTerm $ marginalia (pretty src <+> symDivDiag) <+> aMessage msg

logData :: Pretty a => String -> a -> Pan ()
logData label a = 
  putTerm $ marginalia (divider label) <\\> pretty a <> "\n"

logError :: Error -> Pan ()
logError err =
  putTerm $ anError (divider "ERROR") <\\> pretty err <> "\n"

-- TODO:
-- logWarning :: Pretty a => a -> Pan ()
