module Panini.Elab.Module where

import Data.Text (Text)
import Panini.Pretty
import Panini.Syntax
import Prelude

-- TODO: close sourceType with list of known types (maybe w/ escape hatch) or
-- otherwise use some standard file type descriptor, e.g., Apple UTI.

-------------------------------------------------------------------------------

-- | A 'Module' represents a Panini program loaded from a particular origin.
data Module = Module
  { moduleOrigin :: ModuleOrigin
  , sourceType   :: Text
  , program      :: Program
  }
  deriving stock (Show)

-- | Two modules are considered equal if they share the same origin, i.e., they
-- originate from the same 'File' or are both 'Stdin' or 'REPL' modules,
-- /regardless of their actual program contents./
instance Eq Module where
  x == y = case (x.moduleOrigin, y.moduleOrigin) of
    (File  a, File  b) -> a == b
    (Stdin _, Stdin _) -> True
    (REPL  _, REPL  _) -> True
    _                  -> False

instance Pretty Module where
  pretty = pretty . moduleOrigin

data ModuleOrigin = File FilePath | Stdin Text | REPL Text
  deriving stock (Eq, Ord, Show)

instance Pretty ModuleOrigin where
  pretty = \case
    File  f -> pretty f
    Stdin _ -> "<stdin>"
    REPL  _ -> "<repl>"
