module Panini.Panic where

import GHC.Stack
import Panini.Pretty
import Panini.Version
import Prelude
import Control.Exception

panic :: HasCallStack => Doc -> a
panic msg = errorWithoutStackTrace $ mconcat
  [ "panic! ", showPretty msg, "\n\n"
  , version, "\n"
  , init $ unlines $ tail $ lines $ prettyCallStack callStack
  ]

impossible :: HasCallStack => a
impossible = withFrozenCallStack $ panic "the impossible happened!"

assertM :: HasCallStack => Monad m => Bool -> m ()
assertM p = withFrozenCallStack $ assert p (return ())
