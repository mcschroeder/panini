module Panini.Abstract.AValue
  ( AValue(..)
  , ABool
  , AInt
  , AString
  , AUnit
  , containsTop, containsBot
  , fillTop, fillBot
  , typeOfAValue
  ) where

import Algebra.Lattice
import Data.Hashable
import GHC.Generics (Generic)
import Panini.Abstract.AUnit
import Panini.Abstract.ABool
import Panini.Abstract.AInt
import Panini.Abstract.AString
import Panini.Pretty
import Panini.Syntax.Primitives
import Prelude

------------------------------------------------------------------------------

data AValue
  = AUnit !AUnit
  | ABool !ABool
  | AInt !AInt
  | AString !AString
  deriving stock (Eq, Show, Read, Generic)

instance Hashable AValue

instance Pretty AValue where
  pretty = \case
    AUnit   a -> pretty a
    ABool   a -> pretty a
    AInt    a -> pretty a
    AString a -> pretty a

instance PartialMeetSemilattice AValue where
  AUnit   a ∧? AUnit   b = Just $ AUnit   (a ∧ b)
  ABool   a ∧? ABool   b = Just $ ABool   (a ∧ b)
  AInt    a ∧? AInt    b = Just $ AInt    (a ∧ b)
  AString a ∧? AString b = Just $ AString (a ∧ b)
  _         ∧? _         = Nothing

containsTop :: AValue -> Bool
containsTop = \case
  AUnit   a -> isTop a
  ABool   a -> isTop a
  AInt    a -> isTop a
  AString a -> isTop a

containsBot :: AValue -> Bool
containsBot = \case
  AUnit   a -> isBot a
  ABool   a -> isBot a
  AInt    a -> isBot a
  AString a -> isBot a

fillTop :: AValue -> AValue
fillTop = \case
  AUnit   _ -> AUnit top
  ABool   _ -> ABool top
  AInt    _ -> AInt top
  AString _ -> AString top

fillBot :: AValue -> AValue
fillBot = \case
  AUnit   _ -> AUnit bot
  ABool   _ -> ABool bot
  AInt    _ -> AInt bot
  AString _ -> AString bot

typeOfAValue :: AValue -> Base
typeOfAValue = \case
  AUnit   _ -> TUnit
  ABool   _ -> TBool
  AInt    _ -> TInt
  AString _ -> TString

-- instance Complementable AValue where
--   neg (ABool   a) = ABool   (neg a)
--   neg (AInt    a) = AInt    (neg a)
--   neg (AString _) = undefined -- TODO: AString (neg a)
