module Panini.Abstract.AValue
  ( AValue(..)
  , ABool
  , AInt
  , AString
  , containsTop
  , containsBot
  , typeOfAValue
  ) where

import Algebra.Lattice
import Data.Hashable
import GHC.Generics (Generic)
import Panini.Abstract.ABool
import Panini.Abstract.AInt
import Panini.Abstract.AString
import Panini.Pretty
import Panini.Syntax.Primitives
import Prelude

------------------------------------------------------------------------------

data AValue
  = ABool ABool
  | AInt AInt
  | AString AString
  deriving stock (Eq, Show, Read, Generic)

instance Hashable AValue

instance Pretty AValue where
  pretty = \case
    ABool   a -> pretty a
    AInt    a -> pretty a
    AString a -> pretty a

instance PartialMeetSemilattice AValue where
  ABool   a ∧? ABool   b = Just $ ABool   (a ∧ b)
  AInt    a ∧? AInt    b = Just $ AInt    (a ∧ b)
  AString a ∧? AString b = Just $ AString (a ∧ b)
  _         ∧? _         = Nothing

containsTop :: AValue -> Bool
containsTop = \case
  ABool   a -> isTop a
  AInt    a -> isTop a
  AString a -> isTop a

containsBot :: AValue -> Bool
containsBot = \case
  ABool   a -> isBot a
  AInt    a -> isBot a
  AString a -> isBot a

typeOfAValue :: AValue -> Base
typeOfAValue = \case
  ABool   _ -> TBool
  AInt    _ -> TInt
  AString _ -> TString

-- instance Complementable AValue where
--   neg (ABool   a) = ABool   (neg a)
--   neg (AInt    a) = AInt    (neg a)
--   neg (AString _) = undefined -- TODO: AString (neg a)
