module Panini.Algebra.Lattice where

import Data.Foldable
import Prelude

-------------------------------------------------------------------------------

-- TODO: should be super class of lattices
class PartialOrd a where
  (≤) :: a -> a -> Bool

-------------------------------------------------------------------------------

-- | A meet-semilattice is a partially ordered set where for all elements @a@
-- and @b@ the greatest lower bound @a '∧' b@ (also called the 'meet') exists.
--
-- The binary operation '∧' needs to satisfy the following laws:
--
-- [Idempotency]   @x '∧' x == x@ 
-- [Commutativity] @x '∧' y == y '∧' x@
-- [Associativity] @x '∧' (y '∧' z) = (x '∧' y) '∧' z@
class MeetSemilattice a where
  -- | The 'meet' (greatest lower bound) of two elements.
  (∧) :: a -> a -> a

-- | A synonym for '∧'.
meet :: MeetSemilattice a => a -> a -> a
meet = (∧)

meets1 :: (Foldable t, MeetSemilattice a) => t a -> a
meets1 = foldr1 (∧)

-- | A join-semilattice is a partially ordered set where for all elements @a@
-- and @b@ the least upper bound @a '∨' b@ (also called the 'join') exists.
--
-- The binary operation '∨' needs to satisfy the following laws:
--
-- [Idempotency]   @x '∨' x == x@ 
-- [Commutativity] @x '∨' y == y '∨' x@
-- [Associativity] @x '∨' (y '∨' z) = (x '∨' y) '∨' z@
class JoinSemilattice a where
  -- | The 'join' (least upper bound) of two elements.
  (∨) :: a -> a -> a

-- | A synonym for '∨'.
join :: JoinSemilattice a => a -> a -> a
join = (∨)

joins1 :: (Foldable t, JoinSemilattice a) => t a -> a
joins1 = foldr1 (∨)

-- | A lattice is a partially ordered set that is both a meet- and a
-- join-semilattice and satisfies the following identities:
--
-- [Absorption of meet by join] @a '∨' (a '∧' b) == a@
-- [Absorption of join by meet] @a '∧' (a '∨' b) == a@
type Lattice a = (MeetSemilattice a, JoinSemilattice a)

-------------------------------------------------------------------------------

-- | A bounded meet-semilattice has an additional greatest element 'top' which
-- is the identity element for the 'meet' operation:
--
-- [Meet identity] @a '∧' 'top' == a@
class MeetSemilattice a => BoundedMeetSemilattice a where
  -- | The greatest element (maximum).
  top :: a

-- | Is the given element the greatest element ('top')?
isTop :: (BoundedMeetSemilattice a, Eq a) => a -> Bool
isTop = (top ==)

meets :: (Foldable t, BoundedMeetSemilattice a) => t a -> a
meets = foldr (∧) top

-- | A bounded join-semilattice has an additional least element 'bot' which
-- is the identity element for the 'join' operation:
--
-- [Join identity] @a '∨' 'bot' == a@
class JoinSemilattice a => BoundedJoinSemilattice a where
  -- | The least element (minimum).
  bot :: a

-- | Is the given element the least element ('bot')?
isBot :: (BoundedJoinSemilattice a, Eq a) => a -> Bool
isBot = (bot ==)

joins :: (Foldable t, BoundedJoinSemilattice a) => t a -> a
joins = foldr (∨) bot

-- | A bounded lattice is both a bounded meet- and a bounded join-semilattice.
type BoundedLattice a = (BoundedMeetSemilattice a, BoundedJoinSemilattice a)

-------------------------------------------------------------------------------

-- | A bounded lattice is complemented if every element @a@ has a complement
-- @'neg' a@, such that:
--
-- [Meet complement] @a '∧' 'neg' a == 'bot'@
-- [Join complement] @a '∨' 'neg' a == 'top'@
class BoundedLattice a => ComplementedLattice a where
  neg :: a -> a

-------------------------------------------------------------------------------

-- | A partial meet-semilattice does not have a defined 'meet' for every pair of
-- elements. This is sometimes useful for composite objects.
class PartialMeetSemilattice a where
  (∧?) :: a -> a -> Maybe a

partialMeets :: (Foldable t, PartialMeetSemilattice a) => t a -> [a]
partialMeets = foldr go [] . toList
  where
    go x []     = [x]
    go x (y:ys) = case x ∧? y of
      Just z  -> z : ys
      Nothing -> y : go x ys

-- | A partial join-semilattice does not have a defined 'join' for every pair of
-- elements. This is sometimes useful for composite objects.
class PartialJoinSemilattice a where
  (∨?) :: a -> a -> Maybe a

-- | A partial lattice is both a partial meet- and a partial join-semilattice.
type PartialLattice a = (PartialMeetSemilattice a, PartialJoinSemilattice a)
