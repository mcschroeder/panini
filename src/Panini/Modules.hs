module Panini.Modules
  ( Module
  , getModule
  , moduleRelativeTo
  , moduleLocation
  , replModule
  , stdinModule
  ) where

import Prelude
import System.FilePath
import System.Directory
import Panini.Pretty.Printer

-------------------------------------------------------------------------------

newtype Module = Module FilePath
  deriving stock (Show, Read, Eq, Ord)
  deriving newtype Pretty

getModule :: FilePath -> IO Module
getModule fp = 
  Module <$> normalise <$> makeRelativeToCurrentDirectory fp

moduleRelativeTo :: Module -> FilePath -> Module
moduleRelativeTo (Module this) that =
  Module $ normalise $ takeDirectory this </> that

moduleLocation :: Module -> FilePath
moduleLocation (Module fp) = fp

replModule :: Module
replModule = Module "<repl>"

stdinModule :: Module
stdinModule = Module "<stdin>"
