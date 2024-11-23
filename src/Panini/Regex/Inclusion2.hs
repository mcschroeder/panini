{-

References:

    * Keil, Matthias and Peter Thiemann. 2014. "Symbolic Solving of Extended
    Regular Expression Inequalities." https://arxiv.org/abs/1410.3227

-}
module Panini.Regex.Inclusion2 (isIncludedBy2) where

import Algebra.Lattice
import Control.Exception
import Data.Foldable
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Panini.Regex.CharSet (CharSet)
import Panini.Regex.CharSet qualified as CS
import Panini.Regex.Type
import Panini.Regex.Derivative
import Prelude
import Panini.Panic

-- | The '⊑' type is a reified regular expression inclusion relation.
data a ⊑ b = a :⊑ b 
  deriving stock (Eq, Ord, Show, Read)

infix 0 :⊑


(∈) :: Eq a => Foldable t => a -> t a -> Bool
(∈) = elem

ν = nullable

isIncludedBy2 :: Regex -> Regex -> Bool
isIncludedBy2 r0 s0 = go mempty [r0 :⊑ s0]
 where
  go _ [] = True
  go g (i@(r :⊑ s):t)
    | i ∈ g = go g t
    | not (ν r ==> ν s) = False
    | s == Zero, any (not . CS.null) (next r) = False        
    | r == Zero = go (Set.insert i g) t
    | r == One, ν s = go (Set.insert i g) t
    | r == s = go (Set.insert i g) t        
    | otherwise = go (Set.insert i g) (ps ++ t)
        where
          ps = [(derivative c r :⊑ derivative c s) | a <- Set.toList $ next' i, Just c <- [CS.choose a]]

-- decide :: Set (Regex ⊑ Regex) -> (Regex ⊑ Regex) -> Bool
-- decide g i@(r :⊑ s)
--   | r == s = True -- IDENTITY
--   | r == Zero = True -- PROVE-EMPTY
--   | r == One, ν s = True -- PROVE-NULLABLE
--   | s == Zero, any (not . CS.null) (next r) = False -- DISPROVE-EMPTY
  
--   | ν r, not (ν s) = False -- DISPROVE  
--   | i ∈ g = True -- CYCLE
--   | ν r ==> ν s = let g' = Set.insert i g in and [decide g' (derivative c r :⊑ derivative c s) | a <- Set.toList $ next' i, Just c <- [CS.choose a]]
--   | otherwise = impossible -- either CYCLE or DISPROVE must have fired


(==>) :: Bool -> Bool -> Bool
a ==> b = (not a) || b

next' :: (Regex ⊑ Regex) -> Set CharSet
next' (r :⊑ s) = next r ⋉ next s
