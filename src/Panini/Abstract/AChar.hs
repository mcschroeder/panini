module Panini.Abstract.AChar
  ( AChar
  , count
  , values
  , member
  , choose
  , eq
  , ne
  , toFiniteSet
  , fromFiniteSet
  ) where

import Algebra.Lattice
import Data.Hashable
import Data.IntSet (IntSet)
import Data.IntSet qualified as I
import GHC.Generics
import Panini.Pretty
import Prelude

-------------------------------------------------------------------------------

import Data.Set qualified as S
import Data.GSet (FiniteSet(..)) -- from regexp

toFiniteSet :: AChar -> FiniteSet Char
toFiniteSet (AChar True xs) = These $ intSetToSetOfChar xs
toFiniteSet (AChar False xs) = ComplementOf $ intSetToSetOfChar xs

intSetToSetOfChar :: IntSet -> S.Set Char
intSetToSetOfChar = S.fromList . map (toEnum @Char) . I.toList

fromFiniteSet :: FiniteSet Char -> AChar
fromFiniteSet (These xs) = AChar True $ setOfCharToIntSet xs
fromFiniteSet (ComplementOf xs) = AChar False $ setOfCharToIntSet xs

setOfCharToIntSet :: S.Set Char -> IntSet
setOfCharToIntSet =  I.fromList . map (fromEnum @Char) . S.toList

-------------------------------------------------------------------------------

-- | An abstract character.
data AChar = AChar Bool IntSet
  deriving stock (Eq, Ord, Generic, Show, Read)

instance Hashable AChar

instance MeetSemilattice AChar where
  AChar True  xs ∧ AChar True  ys = AChar True  (I.intersection xs ys)
  AChar False xs ∧ AChar False ys = AChar False (I.union xs ys)
  AChar True  xs ∧ AChar False ys = AChar True  (I.difference xs ys)
  AChar False xs ∧ AChar True  ys = AChar True  (I.difference ys xs)

instance BoundedMeetSemilattice AChar where
  top = AChar False mempty

instance JoinSemilattice AChar where
  AChar True  xs ∨ AChar True  ys = AChar True  (I.union xs ys)
  AChar False xs ∨ AChar False ys = AChar False (I.intersection xs ys)
  AChar True  xs ∨ AChar False ys = AChar False (I.difference ys xs)
  AChar False xs ∨ AChar True  ys = AChar False (I.difference xs ys)

instance BoundedJoinSemilattice AChar where
  bot = AChar True mempty

instance ComplementedLattice AChar where
  neg (AChar b xs) = AChar (not b) xs

toCharList :: IntSet -> [Char]
toCharList = map (toEnum @Char) . I.toAscList

-- | The number of concrete values represented by the abstract character.
count :: AChar -> Int
count (AChar True  cs) = I.size cs
count (AChar False cs) = fromEnum @Char maxBound - I.size cs

-- | The concrete values represented by the abstract character.
values :: AChar -> [Char]
values (AChar True  cs) = toCharList cs
values (AChar False cs) = 
  filter (\x -> fromEnum x `I.notMember` cs) $ enumFromTo minBound maxBound

member :: Char -> AChar -> Bool
member c (AChar True  cs) = I.member    (fromEnum @Char c) cs
member c (AChar False cs) = I.notMember (fromEnum @Char c) cs

choose :: AChar -> Maybe Char
choose cs = case values cs of
  []  -> Nothing
  x:_ -> Just x

-- | An abstract character @= c@.
eq :: Char -> AChar
eq = AChar True . I.singleton . fromEnum

-- | An abstract character @≠ c@, i.e., @= Σ\c@.
ne :: Char -> AChar
ne = AChar False . I.singleton . fromEnum

instance Pretty AChar where
  pretty (AChar True cs) = case toCharList cs of
    []  -> emptySet
    [x] -> pretty x
    xs  -> prettySetTight xs
  
  pretty (AChar False cs) = case toCharList cs of
    []  -> bigSigma
    [x] -> parens $ bigSigma <> setMinus <> pretty x
    xs  -> parens $ bigSigma <> setMinus <> prettySetTight xs
