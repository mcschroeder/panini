module Panini.Regex.Simplify.Press where

import Panini.Regex.Inclusion
import Panini.Regex.Simplify.Common
import Panini.Regex.Type
import Prelude

press :: Context -> Regex -> Regex
press _ = \case
  
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

-- | Checks whether L(r) = L(r*) by testing whether L(rr) ⊆ L(r).
selfStarEq :: Regex -> Bool
selfStarEq r
  | not (nullable r) = False
  | otherwise = case (r <> r) `isUnambiguouslyIncludedBy` r of
      Just b  -> b
      Nothing -> (r <> r) `isIncludedBy` r

-- | @x `isSubsumedBy` y@ returns 'True' iff L(x) ⊆ L(y).
isSubsumedBy :: Regex -> Regex -> Bool
x `isSubsumedBy` y = 
  case x `isUnambiguouslyIncludedBy` y of
    Just b  -> b
    Nothing -> x `isIncludedBy` y
