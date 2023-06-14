module Panini.Abstract.AValue where

import Data.Hashable
import GHC.Generics (Generic)
import Prelude
import Panini.Abstract.ABool
import Panini.Abstract.AInt
import Panini.Abstract.AString
import Panini.Algebra.Lattice
import Panini.Pretty.Printer

------------------------------------------------------------------------------

data AValue
  = ABool ABool
  | AInt AInt
  | AString AString
  deriving stock (Eq, Show, Read, Generic)

instance Hashable AValue

instance Pretty AValue where
  pretty = \case
    ABool a -> pretty a
    AInt a -> pretty a
    AString a -> pretty a

instance PartialMeetSemilattice AValue where
  ABool   a ∧? ABool   b = Just $ ABool   (a ∧ b)
  AInt    a ∧? AInt    b = Just $ AInt    (a ∧ b)
  AString a ∧? AString b = Just $ AString (a ∧ b)
  _         ∧? _         = Nothing

instance Complementable AValue where
  neg (ABool   a) = ABool   (neg a)
  neg (AInt    a) = AInt    (neg a)
  neg (AString _) = undefined -- TODO: AString (neg a)
