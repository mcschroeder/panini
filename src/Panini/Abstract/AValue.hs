module Panini.Abstract.AValue
  ( AValue(..)
  , ABool
  , AInt
  , AChar
  , AString
  , AUnit
  , hasTop, hasBot
  , fillTop, fillBot
  , topValue, botValue
  , typeOfAValue
  , fromValue
  ) where

import Algebra.Lattice
import Data.Data (Data)
import Data.Generics.Uniplate.Direct
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
  | AChar !AChar
  | AString !AString
  deriving stock 
    ( Eq
    , Ord  -- ^ structural ordering
    , Show, Read
    , Generic, Data)

instance Hashable AValue

instance Uniplate AValue where
  uniplate = plate

instance Pretty AValue where
  pretty = \case
    AUnit   a -> pretty a
    ABool   a -> pretty a
    AInt    a -> pretty a
    AChar   a -> pretty a
    AString a -> pretty a

instance PartialOrder AValue where
  AUnit   a ⊑ AUnit   b = a ⊑ b
  ABool   a ⊑ ABool   b = a ⊑ b
  AInt    a ⊑ AInt    b = a ⊑ b
  AChar   a ⊑ AChar   b = a ⊑ b
  AString a ⊑ AString b = a ⊑ b
  _         ⊑ _         = False

instance PartialMeetSemilattice AValue where
  AUnit   a ∧? AUnit   b = Just $ AUnit   (a ∧ b)
  ABool   a ∧? ABool   b = Just $ ABool   (a ∧ b)
  AInt    a ∧? AInt    b = Just $ AInt    (a ∧ b)
  AChar   a ∧? AChar   b = Just $ AChar   (a ∧ b)
  AString a ∧? AString b = Just $ AString (a ∧ b)
  _         ∧? _         = Nothing

instance PartialJoinSemilattice AValue where
  AUnit   a ∨? AUnit   b = Just $ AUnit   (a ∨ b)
  ABool   a ∨? ABool   b = Just $ ABool   (a ∨ b)
  AInt    a ∨? AInt    b = Just $ AInt    (a ∨ b)
  AChar   a ∨? AChar   b = Just $ AChar   (a ∨ b)
  AString a ∨? AString b = Just $ AString (a ∨ b)
  _         ∨? _         = Nothing

hasTop :: AValue -> Bool
hasTop = \case
  AUnit   a -> isTop a
  ABool   a -> isTop a
  AInt    a -> isTop a
  AChar   a -> isTop a
  AString a -> isTop a

hasBot :: AValue -> Bool
hasBot = \case
  AUnit   a -> isBot a
  ABool   a -> isBot a
  AInt    a -> isBot a
  AChar   a -> isBot a
  AString a -> isBot a

fillTop :: AValue -> AValue
fillTop = topValue . typeOfAValue

fillBot :: AValue -> AValue
fillBot = botValue . typeOfAValue

topValue :: Base -> AValue
topValue = \case
  TUnit   -> AUnit   top
  TBool   -> ABool   top
  TInt    -> AInt    top
  TChar   -> AChar   top
  TString -> AString top

botValue :: Base -> AValue
botValue = \case
  TUnit   -> AUnit   bot
  TBool   -> ABool   bot
  TInt    -> AInt    bot
  TChar   -> AChar   bot
  TString -> AString bot


typeOfAValue :: AValue -> Base
typeOfAValue = \case
  AUnit   _ -> TUnit
  ABool   _ -> TBool
  AInt    _ -> TInt
  AChar   _ -> TChar
  AString _ -> TString

fromValue :: Value -> AValue
fromValue = \case
  U   _ -> AUnit Unit
  B b _ -> ABool $ ABool.eq b
  I i _ -> AInt $ AInt.eq i
  C c _ -> AChar $ AChar.eq c
  S t _ -> AString $ AString.eq $ Text.unpack t
