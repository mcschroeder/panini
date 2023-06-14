module Panini.Solver.Abstract.AChar
  ( AChar
  , concreteSize
  , concreteValues
  , aCharEq
  , aCharNe
  -- , toPred
  , aCharToFiniteSet
  , finiteSetToAChar
  ) where

import Data.Hashable
import Data.IntSet (IntSet)
import Data.IntSet qualified as I
import GHC.Generics
import Panini.Algebra.Lattice
import Panini.Pretty.Printer
-- import Panini.Syntax
import Prelude
-- import Data.Text qualified as Text

-------------------------------------------------------------------------------

import Data.Set qualified as S
import Data.GSet -- from regexp

aCharToFiniteSet :: AChar -> FiniteSet Char
aCharToFiniteSet (AChar True xs) = These $ intSetToSetOfChar xs
aCharToFiniteSet (AChar False xs) = ComplementOf $ intSetToSetOfChar xs

intSetToSetOfChar :: IntSet -> S.Set Char
intSetToSetOfChar = S.fromList . map (toEnum @Char) . I.toList

finiteSetToAChar :: FiniteSet Char -> AChar
finiteSetToAChar (These xs) = AChar True $ setOfCharToIntSet xs
finiteSetToAChar (ComplementOf xs) = AChar False $ setOfCharToIntSet xs

setOfCharToIntSet :: S.Set Char -> IntSet
setOfCharToIntSet =  I.fromList . map (fromEnum @Char) . S.toList

-------------------------------------------------------------------------------

-- | An abstract character.
data AChar = AChar Bool IntSet
  deriving stock (Eq, Generic, Show, Read)

instance Hashable AChar

instance MeetSemilattice AChar where
  AChar True  xs ∧ AChar True  ys = AChar True  (I.intersection xs ys)
  AChar False xs ∧ AChar False ys = AChar False (I.union xs ys)
  AChar True  xs ∧ AChar False ys = AChar True  (I.difference xs ys)
  AChar False xs ∧ AChar True  ys = AChar True  (I.difference ys xs)

instance BoundedMeetSemilattice AChar where
  (⊤) = AChar False mempty

instance JoinSemilattice AChar where
  AChar True  xs ∨ AChar True  ys = AChar True  (I.union xs ys)
  AChar False xs ∨ AChar False ys = AChar False (I.intersection xs ys)
  AChar True  xs ∨ AChar False ys = AChar False (I.difference ys xs)
  AChar False xs ∨ AChar True  ys = AChar False (I.difference xs ys)

instance BoundedJoinSemilattice AChar where
  (⊥) = AChar True mempty

instance Complementable AChar where
  neg (AChar b xs) = AChar (not b) xs

instance ComplementedLattice AChar

toCharList :: IntSet -> [Char]
toCharList = map (toEnum @Char) . I.toAscList

-- | The number of concrete values represented by the abstract character.
concreteSize :: AChar -> Int
concreteSize (AChar True  cs) = I.size cs
concreteSize (AChar False cs) = fromEnum @Char maxBound - I.size cs

-- | The concrete values represented by the abstract character.
concreteValues :: AChar -> [Char]
concreteValues (AChar True  cs) = toCharList cs
concreteValues (AChar False cs) = 
  filter (\x -> fromEnum x `I.notMember` cs) $ enumFromTo minBound maxBound

-- | An abstract character @= c@.
aCharEq :: Char -> AChar
aCharEq = AChar True . I.singleton . fromEnum

-- | An abstract character @≠ c@, i.e., @= Σ\c@.
aCharNe :: Char -> AChar
aCharNe = AChar False . I.singleton . fromEnum

-- toPred :: PExpr -> AChar -> Pred
-- toPred lhs (AChar b cs) = case b of
--   True  -> joins $ map (mkRel Eq) $ toCharList cs
--   False -> meets $ map (mkRel Ne) $ toCharList cs
--  where
--   mkRel r c = PRel r lhs (PCon (S (Text.singleton c) NoPV))  

instance Pretty AChar where
  pretty (AChar True cs) = case toCharList cs of
    []  -> "∅"
    [x] -> pretty x
    xs  -> prettySetTight xs
  
  pretty (AChar False cs) = case toCharList cs of
    []  -> "Σ"
    [x] -> parens $ "Σ∖" <> pretty x
    xs  -> parens $ "Σ∖" <> prettySetTight xs
