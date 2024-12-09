module Regex.Simplify.Press where

import Control.Applicative
import Data.List.Extra (firstJust)
import Prelude
import Regex.Equivalence
import Regex.Inclusion
import Regex.Simplify.Common
import Regex.Type

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

  -- x + y = (x ↘ y) + y ----------------------------------------
  Plus xs0 -> Plus $ go [] xs0
   where
    go ys []                                 = ys
    go ys (x:xs)
      | Just y <- firstJust (x ↘) (xs ++ ys) = go (y:ys) xs
      | otherwise                            = go (x:ys) xs
    
  ---------------------------------------------------------------
  r -> r

-- | Checks whether L(r) = L(r*) by testing whether L(ε) ⊆ L(rr) ⊆ L(r).
selfStarEq :: Regex -> Bool
selfStarEq r = nullable r && (r <> r) ⊑ r

-- | If we have a choice x + y we can use the operation x ↘ y to try to "press"
-- x into a smaller expression x' such that L(x) ∖ L(y) ⊆ L(x') ⊆ L(x + y).
-- In the best case, L(x) ⊆ L(y) and thus x ↘ y = ∅ so that x + y = y.
(↘) :: Regex -> Regex -> Maybe Regex
x ↘ y       | x ⊑ y             = Just Zero
x ↘ Opt y   | not (nullable x)  = y ↘ x
Opt x ↘ y   | nullable y        = x ↘ y <|> Just x
Star x ↘ y  | Opt x ≡ Star x    = Opt x ↘ y
x ↘ Star y                      = x ↘ y
_ ↘ _                           = Nothing

-- | @x '≡' y@ tests whether L(x) = L(y). Warning: this can be expensive!
(≡) :: Regex -> Regex -> Bool
r ≡ s = equivalence r s

-- | @x '⊑' y@ tests whether L(x) ⊆ L(y). Warning: this can be expensive!
(⊑) :: Regex -> Regex -> Bool
r ⊑ s = case r `isUnambiguouslyIncludedBy` s of
  Just b -> b
  Nothing -> r `isIncludedBy` s
