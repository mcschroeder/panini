-- | This module exports types and functions to work with extended regular
-- expressions (i.e., regexes that permit intersection and complement).
module Panini.Regex
  ( Regex(..)
  , pattern Zero
  , pattern All
  , pattern AnyChar
  , pattern Times
  , pattern Plus
  , pattern Star
  , pattern Opt
  , intersection
  , complement
  , simplify
  , equivalence
  , membership
  ) where

import Panini.Regex.Equivalence
import Panini.Regex.Operations
import Panini.Regex.Simplify
import Panini.Regex.Type
import Prelude ()