-- | This module exports types and functions to work with extended regular
-- expressions (i.e., regexes that permit intersection and complement).
module Regex
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

import Regex.Equivalence
import Regex.Operations
import Regex.Simplify
import Regex.Type
import Prelude ()