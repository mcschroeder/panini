module Panini.Abstract.AValue
  ( AValue(..)
  , ABool
  , AInt
  , AString
  , AUnit
  , containsTop, containsBot
  , fillTop, fillBot
  , typeOfAValue
  , fromValue
  ) where

import Algebra.Lattice
import Data.Hashable
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Panini.Abstract.ABool as ABool
import Panini.Abstract.AChar as AChar
import Panini.Abstract.AInt as AInt
import Panini.Abstract.AString as AString
import Panini.Abstract.AUnit
import Panini.Pretty
import Panini.Syntax.Primitives
import Prelude

------------------------------------------------------------------------------

data AValue
  = AUnit !AUnit
  | ABool !ABool
  | AInt !AInt
  | AString !AString
  deriving stock 
    ( Eq
    , Ord  -- ^ structural ordering
    , Show, Read, Generic)

instance Hashable AValue

instance Pretty AValue where
  pretty = \case
    AUnit   a -> pretty a
    ABool   a -> pretty a
    AInt    a -> pretty a
    AString a -> pretty a

instance PartialOrder AValue where
  AUnit   a ⊑ AUnit   b = a ⊑ b
  ABool   a ⊑ ABool   b = a ⊑ b
  AInt    a ⊑ AInt    b = a ⊑ b
  AString a ⊑ AString b = a ⊑ b
  _         ⊑ _         = False

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

fromValue :: Value -> AValue
fromValue = \case
  U   _ -> AUnit Unit
  B b _ -> ABool $ ABool.eq b
  I i _ -> AInt $ AInt.eq i
  S t _ -> case Text.unpack t of
    [c] -> AString $ lit (AChar.eq c)  -- TODO: assumes all singleton strings are chars
    s   -> AString $ AString.eq s
