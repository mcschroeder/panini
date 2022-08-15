module Panini.Solver.Abstract.ABool
  ( ABool
  , concreteBool
  , aBoolEq
  ) where

import Panini.Pretty.Printer
import Panini.Solver.Abstract.Lattice
import Prelude

-------------------------------------------------------------------------------

-- | An abstract Boolean value.
data ABool
  = Top
  | True_
  | False_
  | Bottom
  deriving stock (Eq, Show, Read)

instance JoinSemilattice ABool where
  Top    ⊔ _      = Top
  _      ⊔ Top    = Top
  True_  ⊔ False_ = Top
  False_ ⊔ True_  = Top
  True_  ⊔ True_  = True_
  False_ ⊔ False_ = False_
  Bottom ⊔ x      = x
  x      ⊔ Bottom = x

instance BoundedJoinSemilattice ABool where
  (⊥) = Bottom

instance MeetSemilattice ABool where
  Bottom ⊓ _      = Bottom
  _      ⊓ Bottom = Bottom
  True_  ⊓ False_ = Bottom
  False_ ⊓ True_  = Bottom
  True_  ⊓ True_  = True_
  False_ ⊓ False_ = False_
  Top    ⊓ x      = x
  x      ⊓ Top    = x

instance BoundedMeetSemilattice ABool where
  (⊤) = Top

-- | The single concrete value represented by the abstract Boolean, or Nothing.
concreteBool :: ABool -> Maybe Bool
concreteBool True_  = Just True
concreteBool False_ = Just False
concreteBool _      = Nothing

-- | An abstract Boolean equal to this concrete value.
aBoolEq :: Bool -> ABool
aBoolEq True  = True_
aBoolEq False = False_

instance Pretty ABool where
  pretty Top = "{𝗧,𝗙}"
  pretty True_ = "𝗧" 
  pretty False_ = "𝗙"
  pretty Bottom = "↯"
