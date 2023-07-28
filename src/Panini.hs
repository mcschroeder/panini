module Panini
  ( module Panini.Monad
  , loadFile
  ) where

import Panini.Monad
import Prelude
import Data.Text.IO qualified as Text
import Panini.Parser (parseProgram)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Panini.Language.Elaborator
import Panini.Logger

loadFile :: FilePath -> Pan ()
loadFile path = do
  logMessage "Panini" $ "Read " ++ path
  src <- liftIO $ Text.readFile path  -- TODO: handle exception
  logData (path ++ " (Raw Source)") src
  logMessage "Parser" $ "Parse " ++ path
  prog <- lift $ except $ parseProgram path src
  logData (path ++ " (Parsed)") prog
  elaborateProgram path prog
