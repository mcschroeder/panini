module Panini
  ( module Panini.Monad
  , loadFile
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Maybe
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Panini.Language.Elaborator
import Panini.Logger
import Panini.Modules
import Panini.Monad
import Panini.Parser (parseProgram)
import Panini.Provenance
import Prelude

------------------------------------------------------------------------------

-- | Load the file at the given path into the Panini environment.
loadFile :: FilePath -> Pan ()
loadFile path = do
  -- TODO: check if file already loaded!
  curMod <- tryIO NoPV $ findModule path Nothing []
  case curMod of
    Nothing -> undefined -- TODO
    Just curMod' -> do
      let path' = fromJust $ modulePath curMod'
      logMessage "Panini" $ "Read " ++ path'
      src <- tryIO NoPV $ Text.readFile path'
      logData (path ++ " (Raw Source)") src
  
      logMessage "Parser" $ "Parse " ++ path'
      prog <- lift $ except $ parseProgram path' src
      logData (path' ++ " (Parsed)") prog
  
      elaborateProgram curMod' prog

-- | Load the contents of 'stdin' (until @EOF@) into the Panini environment.
loadStdin :: Pan ()
loadStdin = do
  logMessage "Panini" $ "Read stdin"
  src <- tryIO NoPV $ Text.getContents
  logData ("<stdin> (Raw Source)") src
  
  undefined 