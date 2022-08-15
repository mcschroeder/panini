module Panini.Solver.Abstract.Lattice where

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
