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
import Data.Coerce
import Data.Data (Data)
import Data.Hashable
import GHC.Generics
import Panini.Pretty
import Prelude
import Regex.CharSet (CharSet)
import Regex.CharSet qualified as CS

-------------------------------------------------------------------------------

-- | An abstract character.
newtype AChar = AChar CharSet
  deriving stock (Generic, Show, Read, Data)
  deriving newtype (Eq, Ord)

instance PartialOrder           AChar where (⊑) = coerce CS.isSubsetOf
instance MeetSemilattice        AChar where (∧) = coerce CS.intersection
instance JoinSemilattice        AChar where (∨) = coerce CS.union
instance BoundedMeetSemilattice AChar where top = coerce CS.full
instance BoundedJoinSemilattice AChar where bot = coerce CS.empty
instance ComplementedLattice    AChar where neg = coerce CS.complement


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
  pretty (AChar cs) = CS.prettyCharSet cs

toCharSet :: AChar -> CharSet
toCharSet (AChar cs) = cs

fromCharSet :: CharSet -> AChar
fromCharSet = AChar
