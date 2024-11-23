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
simplify = converge $ apply Free lookup 
                    . apply Free press  
                    . apply Free lift  
                    . apply Free factor  
                    . apply Free fuse

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

apply :: Context -> (Context -> Regex -> Regex) -> Regex -> Regex
apply ctx f = \case
  r@(Plus xs)
    | nullable r  -> f ctx $ Plus $ map (apply (max ctx Optional) f) xs
    | otherwise   -> f ctx $ Plus $ map (apply ctx f) xs
  Times xs        -> f ctx $ Times $ map (apply Free f) xs
  Opt r           -> f ctx $ Opt $ apply (max ctx Optional) f r
  Star r          -> f ctx $ Star $ apply Starred f r
  r               -> r
