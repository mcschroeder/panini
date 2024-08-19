module Panini.Abstract.AChar
  ( AChar
  , count
  , values
  , member
  , choose
  , eq
  , ne
  , toCharSet
  , fromCharSet
  ) where

import Algebra.Lattice
import Data.Data (Data)
import Data.Hashable
import GHC.Generics
import Panini.Pretty
import Panini.Regex.CharSet (CharSet)
import Panini.Regex.CharSet qualified as CS
import Prelude

-------------------------------------------------------------------------------

-- | An abstract character.
newtype AChar = AChar CharSet
  deriving stock (Generic, Show, Read, Data)
  deriving newtype 
    ( Eq, Ord
    , PartialOrder
    , MeetSemilattice, JoinSemilattice
    , BoundedMeetSemilattice, BoundedJoinSemilattice
    , ComplementedLattice
    )

instance Hashable AChar

-- | The number of concrete values represented by the abstract character.
count :: AChar -> Int
count (AChar cs) = CS.size cs

-- | The concrete values represented by the abstract character.
values :: AChar -> [Char]
values (AChar cs) = CS.toList cs

member :: Char -> AChar -> Bool
member c (AChar cs) = CS.member c cs

choose :: AChar -> Maybe Char
choose (AChar cs) = CS.choose cs

-- | An abstract character @= c@.
eq :: Char -> AChar
eq = AChar . CS.singleton

-- | An abstract character @≠ c@, i.e., @= Σ\c@.
ne :: Char -> AChar
ne = AChar . CS.complement . CS.singleton

instance Pretty AChar where
  pretty (AChar cs) = pretty cs

toCharSet :: AChar -> CharSet
toCharSet (AChar cs) = cs

fromCharSet :: CharSet -> AChar
fromCharSet = AChar
