-- | This module contains types and functions to work with extended regular
-- expressions (i.e., regexes that permit intersection and complement).
--
-- There are some aspects of note:
--
--   1) The constructors of the 'Regex' data type are somewhat optimized for
--      efficiency of representation and thus not mathematically "minimal". For
--      example, choice (+) and sequence (⋅) are n-ary operations, and we
--      include a redundant constructor for optionals (?).
--
--   2) The literals in the 'Regex' data type are character sets ('CharSet')
--      instead of just single characters ('Char'). This enables efficient and
--      succinct representation of character classes (e.g., @[a-z]@).
--
--   3) Operations like 'intersection' and 'normalize' are implemented entirely
--      algebraically, without intermediate translation into automata.
--
module Panini.Regex
  ( Regex(..)
  , pattern Zero
  , pattern One
  , pattern All
  , pattern AnyChar
  , intersection
  , complement
  , simplify
  ) where

import Panini.Regex.Operations
import Panini.Regex.Simplify
import Panini.Regex.Type
import Prelude ()