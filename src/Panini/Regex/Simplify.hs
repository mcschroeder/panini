{-
This module implements regular expression simplifications based on the
transformations from Kahrs and Runcimcan (2022).

References:

  * Kahrs, Stefan and Colin Runciman. 2022. "Simplifying Regular Expressions
    Further." Journal of Symbolic Computation 109 (2022): 124–143.
    https://doi.org/10.1016/j.jsc.2021.08.003

-}
module Panini.Regex.Simplify (simplify) where

import Panini.Regex.Simplify.Common
import Panini.Regex.Simplify.Factor
import Panini.Regex.Simplify.Fuse
import Panini.Regex.Simplify.Lift
import Panini.Regex.Simplify.Lookup
import Panini.Regex.Simplify.Press
import Panini.Regex.Type
import Prelude hiding (lookup)

-------------------------------------------------------------------------------

simplify :: Regex -> Regex
simplify = converge $ go Free
 where
  go ctx = descend ctx . lookup . press . lift ctx . factor . fuse ctx
  descend ctx = \case
    Times xs        -> Times $ map (go Free) xs
    r@(Plus xs)
      | nullable r  -> Plus $ map (go (max ctx Optional)) xs
      | otherwise   -> Plus $ map (go ctx) xs
    Opt r           -> Opt $ go (max ctx Optional) r
    Star r          -> Star $ go Starred r
    r               -> r

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)
