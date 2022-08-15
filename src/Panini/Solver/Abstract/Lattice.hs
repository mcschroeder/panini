module Panini.Solver.Abstract.Lattice where

import Data.Foldable
import Prelude

class MeetSemilattice a where
  (⊓) :: a -> a -> a

meets1 :: (Foldable t, MeetSemilattice a) => t a -> a
meets1 = foldr1 (⊓)

class MeetSemilattice a => BoundedMeetSemilattice a where
  (⊤) :: a

meets :: (Foldable t, BoundedMeetSemilattice a) => t a -> a
meets = foldr (⊓) (⊤)

class JoinSemilattice a where
  (⊔) :: a -> a -> a

joins1 :: (Foldable t, JoinSemilattice a) => t a -> a
joins1 = foldr1 (⊔)

class JoinSemilattice a => BoundedJoinSemilattice a where
  (⊥) :: a

joins :: (Foldable t, BoundedJoinSemilattice a) => t a -> a
joins = foldr (⊔) (⊥)

type Lattice a = (MeetSemilattice a, JoinSemilattice a)
type BoundedLattice a = (BoundedMeetSemilattice a, BoundedJoinSemilattice a)

class PartialMeetSemilattice a where
  (⊓?) :: a -> a -> Maybe a

partialMeets :: (Foldable t, PartialMeetSemilattice a) => t a -> [a]
partialMeets = foldr go [] . toList
  where
    go x []     = [x]
    go x (y:ys) = case x ⊓? y of
      Just z  -> z : ys
      Nothing -> y : go x ys

class PartialJoinSemilattice a where
  (⊔?) :: a -> a -> Maybe a

type PartialLattice a = (PartialMeetSemilattice a, PartialJoinSemilattice a)
