module Panini.Abstract.ABool
  ( ABool
  , value
  , eq
  ) where

import Algebra.Lattice
import Data.Hashable
import GHC.Generics
import Panini.Pretty
import Prelude

-------------------------------------------------------------------------------

-- | An abstract Boolean value.
data ABool
  = Top
  | True_
  | False_
  | Bottom
  deriving stock (Eq, Generic, Show, Read)

instance Hashable ABool

instance JoinSemilattice ABool where
  Top    ∨ _      = Top
  _      ∨ Top    = Top
  True_  ∨ False_ = Top
  False_ ∨ True_  = Top
  True_  ∨ True_  = True_
  False_ ∨ False_ = False_
  Bottom ∨ x      = x
  x      ∨ Bottom = x

instance BoundedJoinSemilattice ABool where
  bot = Bottom

instance MeetSemilattice ABool where
  Bottom ∧ _      = Bottom
  _      ∧ Bottom = Bottom
  True_  ∧ False_ = Bottom
  False_ ∧ True_  = Bottom
  True_  ∧ True_  = True_
  False_ ∧ False_ = False_
  Top    ∧ x      = x
  x      ∧ Top    = x

instance BoundedMeetSemilattice ABool where
  top = Top

instance ComplementedLattice ABool where
  neg Top    = Bottom
  neg True_  = False_
  neg False_ = True_
  neg Bottom = Top

-- | The single concrete value represented by the abstract Boolean, or Nothing.
value :: ABool -> Maybe Bool
value True_  = Just True
value False_ = Just False
value _      = Nothing

-- | An abstract Boolean equal to this concrete value.
eq :: Bool -> ABool
eq True  = True_
eq False = False_

instance Pretty ABool where
  pretty Top    = prettySet [symTrue,symFalse]
  pretty True_  = symTrue
  pretty False_ = symFalse
  pretty Bottom = emptySet
