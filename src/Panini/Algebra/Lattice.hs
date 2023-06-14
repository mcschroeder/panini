module Panini.Algebra.Lattice where

import Data.Foldable
import Prelude

-------------------------------------------------------------------------------

-- TODO: should be super class of lattices
class PartialOrd a where
  (≤) :: a -> a -> Bool

-------------------------------------------------------------------------------

class MeetSemilattice a where
  (∧) :: a -> a -> a

meet :: MeetSemilattice a => a -> a -> a
meet = (∧)

meets1 :: (Foldable t, MeetSemilattice a) => t a -> a
meets1 = foldr1 (∧)

class JoinSemilattice a where
  (∨) :: a -> a -> a

join :: JoinSemilattice a => a -> a -> a
join = (∨)

joins1 :: (Foldable t, JoinSemilattice a) => t a -> a
joins1 = foldr1 (∨)

type Lattice a = (MeetSemilattice a, JoinSemilattice a)

-------------------------------------------------------------------------------

class MeetSemilattice a => BoundedMeetSemilattice a where
  top :: a

isTop :: (BoundedMeetSemilattice a, Eq a) => a -> Bool
isTop = (top ==)

meets :: (Foldable t, BoundedMeetSemilattice a) => t a -> a
meets = foldr (∧) top

class JoinSemilattice a => BoundedJoinSemilattice a where
  bot :: a

isBot :: (BoundedJoinSemilattice a, Eq a) => a -> Bool
isBot = (bot ==)

joins :: (Foldable t, BoundedJoinSemilattice a) => t a -> a
joins = foldr (∨) bot

type BoundedLattice a = (BoundedMeetSemilattice a, BoundedJoinSemilattice a)

-------------------------------------------------------------------------------

-- TODO: change to ComplementedLattice
class Complementable a where
  neg :: a -> a

type ComplementedLattice a = (BoundedLattice a, Complementable a)

-------------------------------------------------------------------------------

class PartialMeetSemilattice a where
  (∧?) :: a -> a -> Maybe a

partialMeets :: (Foldable t, PartialMeetSemilattice a) => t a -> [a]
partialMeets = foldr go [] . toList
  where
    go x []     = [x]
    go x (y:ys) = case x ∧? y of
      Just z  -> z : ys
      Nothing -> y : go x ys

class PartialJoinSemilattice a where
  (∨?) :: a -> a -> Maybe a

type PartialLattice a = (PartialMeetSemilattice a, PartialJoinSemilattice a)
