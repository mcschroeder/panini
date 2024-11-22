module Panini.Regex.Simplify.Press where

import Panini.Regex.Equivalence
import Panini.Regex.Inclusion
import Panini.Regex.Inclusion2
import Panini.Regex.Inclusion3
import Panini.Regex.Simplify.Common
import Panini.Regex.Type
import Prelude

press :: Regex -> Regex
press = \case
  
  -- x?⋅y? = (x + y)*  if L(x?⋅y?) = L((x?⋅y?)*) ----------------
  Times xs0 -> Times $ go [] xs0
   where
    go ys (x:xs)
      | nullable x = go (x:ys) xs
      | otherwise  = tryPress (reverse ys) ++ x : go [] xs
    go ys []       = tryPress (reverse ys)

    tryPress []               = []
    tryPress [y]              = [y]
    tryPress ys
      | selfStarEq (Times ys) = [Star (Plus $ map flatNullable ys)]
      | otherwise             = ys
  
    
  -- (x + y) = y  if L(x) ⊆ L(y) --------------------------------
  Plus xs0 -> Plus $ go [] xs0
   where
    go ys [] = ys
    go ys (x:xs) | (x `isSubsumedBy`) `any` (xs ++ ys) = go ys xs
                 | otherwise = go (x:ys) xs

  ---------------------------------------------------------------
  r -> r

-- | Checks whether L(r) = L(r*) by testing whether L(rr) ⊆ L(r). Falls back to
-- testing proper equivalence if r is 1-ambiguous. If r is 1-ambiguous and too
-- large, this function will return 'False' even if the equivalence might hold.
selfStarEq :: Regex -> Bool
selfStarEq r
  | not (nullable r) = False
  | r2 == r          = True
  -- | otherwise        = r2 `isIncludedBy3` r
  | otherwise        = case r2 `isIncludedBy` r of
                        Yes             -> True
                        No              -> False
                        OneAmbiguous
                          | size r < 80 -> r2 `isIncludedBy2` r
                          | otherwise   -> False
 where
  r2 = r <> r

-- | @x `isSubsumedBy` y@ returns 'True' iff L(x) ⊆ L(y) and y is 1-unambiguous.
isSubsumedBy :: Regex -> Regex -> Bool
-- x `isSubsumedBy` y = x `isIncludedBy3` y
x `isSubsumedBy` y = case x `isIncludedBy` y of
                      Yes          -> True
                      _           -> False
                      -- OneAmbiguous 
                      --   | size x < 80, size y < 80 -> x `isIncludedBy2` y
                      --   | otherwise -> False
