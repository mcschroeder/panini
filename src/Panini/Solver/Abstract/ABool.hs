module Panini.Solver.Abstract.ABool
  ( ABool
  , concreteBool
  , aBoolEq
  ) where

import Algebra.Lattice
import Prelude

-------------------------------------------------------------------------------

-- | An abstract Boolean value.
data ABool
  = Top
  | True_
  | False_
  | Bottom
  deriving stock (Eq, Show, Read)

instance Lattice ABool where
  Top    \/ _      = Top
  _      \/ Top    = Top
  True_  \/ False_ = Top
  False_ \/ True_  = Top
  True_  \/ True_  = True_
  False_ \/ False_ = False_
  Bottom \/ x      = x
  x      \/ Bottom = x

  Bottom /\ _      = Bottom
  _      /\ Bottom = Bottom
  True_  /\ False_ = Bottom
  False_ /\ True_  = Bottom
  True_  /\ True_  = True_
  False_ /\ False_ = False_
  Top    /\ x      = x
  x      /\ Top    = x

instance BoundedJoinSemiLattice ABool where
  bottom = Bottom

instance BoundedMeetSemiLattice ABool where
  top = Top

-- | The single concrete value represented by the abstract Boolean, or Nothing.
concreteBool :: ABool -> Maybe Bool
concreteBool True_  = Just True
concreteBool False_ = Just False
concreteBool _      = Nothing

-- | An abstract Boolean equal to this concrete value.
aBoolEq :: Bool -> ABool
aBoolEq True  = True_
aBoolEq False = False_
