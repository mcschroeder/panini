-- | An efficient implementation of Unicode character sets/classes, in
-- particular with respect to 'complement'.
--
-- This module is largely based on Edward Kmett's "charset" package
-- (https://hackage.haskell.org/package/charset), which unfortunately is too
-- slow for our use case (probably because it stores an extra set of head bytes
-- that need to be re-constructed anytime charsets are joined/meeted).
-- 
module Panini.Regex.CharSet
  ( CharSet
  , empty
  , full
  , singleton
  , complement
  , size
  , toList
  , member
  , choose
  , fromCharSet
  , intSetToCharList
  ) where

import Algebra.Lattice
import Data.Hashable
import Data.IntSet (IntSet)
import Data.IntSet qualified as I
import GHC.Generics
import Prelude

-------------------------------------------------------------------------------

-- | A set of Unicode characters.
data CharSet = CharSet !Bool !IntSet
  deriving stock (Eq, Ord, Generic, Show, Read)

instance Hashable CharSet

instance MeetSemilattice        CharSet where (∧) = intersection
instance JoinSemilattice        CharSet where (∨) = union
instance BoundedMeetSemilattice CharSet where top = full
instance BoundedJoinSemilattice CharSet where bot = empty
instance ComplementedLattice    CharSet where neg = complement

-------------------------------------------------------------------------------

-- | The empty character set (∅).
empty :: CharSet
empty = CharSet True mempty

-- | The set of all characters (Σ).
full :: CharSet
full = CharSet False mempty

-- | A set consisting of a single character.
singleton :: Char -> CharSet
singleton = CharSet True . I.singleton . fromEnum

-- | The complement of a character set.
complement :: CharSet -> CharSet
complement (CharSet b xs) = CharSet (not b) xs

-- | The union of two character sets.
union :: CharSet -> CharSet -> CharSet
union (CharSet True  xs) (CharSet True  ys) = CharSet True  (I.union        xs ys)
union (CharSet False xs) (CharSet False ys) = CharSet False (I.intersection xs ys)
union (CharSet True  xs) (CharSet False ys) = CharSet False (I.difference   ys xs)
union (CharSet False xs) (CharSet True  ys) = CharSet False (I.difference   xs ys)

-- | The intersection of two character sets.
intersection :: CharSet -> CharSet -> CharSet
intersection (CharSet True  xs) (CharSet True  ys) = CharSet True  (I.intersection xs ys)
intersection (CharSet False xs) (CharSet False ys) = CharSet False (I.union        xs ys)
intersection (CharSet True  xs) (CharSet False ys) = CharSet True  (I.difference   xs ys)
intersection (CharSet False xs) (CharSet True  ys) = CharSet True  (I.difference   ys xs)

-- | The number of characters in the set.
size :: CharSet -> Int
size (CharSet True  cs) = I.size cs
size (CharSet False cs) = fromEnum @Char maxBound - I.size cs

-- | The characters in the set, in undefined order.
toList :: CharSet -> [Char]
toList (CharSet True  cs) = intSetToCharList cs
toList (CharSet False cs) = 
  filter (\x -> fromEnum x `I.notMember` cs) $ enumFromTo minBound maxBound

-- | Whether a given character is in the set.
member :: Char -> CharSet -> Bool
member c (CharSet True  cs) = I.member    (fromEnum @Char c) cs
member c (CharSet False cs) = I.notMember (fromEnum @Char c) cs

-- | Choose a random character from the set, or 'Nothing' if it is empty.
choose :: CharSet -> Maybe Char
choose cs = case toList cs of
  []  -> Nothing
  x:_ -> Just x

-------------------------------------------------------------------------------

fromCharSet :: CharSet -> (Bool, IntSet)
fromCharSet (CharSet b xs) = (b,xs)

intSetToCharList :: IntSet -> [Char]
intSetToCharList = map (toEnum @Char) . I.toAscList
