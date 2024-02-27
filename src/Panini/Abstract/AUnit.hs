module Panini.Abstract.AUnit where

import Algebra.Lattice
import Data.Hashable
import GHC.Generics
import Panini.Pretty
import Prelude

-- | An abstract unit type. Basically 'TUnit' with an additional bottom.
data AUnit = Bottom | Unit
  deriving stock (Eq, Ord, Generic, Show, Read)

instance Hashable AUnit

instance PartialOrder AUnit where
  (⊑) = (<=)

instance JoinSemilattice AUnit where
  _      ∨ Unit   = Unit
  Unit   ∨ _      = Unit
  Bottom ∨ Bottom = Bottom

instance BoundedJoinSemilattice AUnit where
  bot = Bottom

instance MeetSemilattice AUnit where
  Bottom ∧ _      = Bottom
  _      ∧ Bottom = Bottom
  Unit   ∧ Unit   = Unit

instance BoundedMeetSemilattice AUnit where
  top = Unit

instance ComplementedLattice AUnit where
  neg Unit   = Bottom
  neg Bottom = Unit

instance Pretty AUnit where
  pretty = ann (Literal AbstractLit) . \case
    Unit   -> symUnit
    Bottom -> symBot
