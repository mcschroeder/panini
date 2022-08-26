module Panini.Solver.Abstract.AChar
  ( AChar
  , concreteSize
  , concreteValues
  , aCharEq
  , aCharNe
  ) where

import Data.Hashable
import Data.IntSet (IntSet)
import Data.IntSet qualified as I
import Data.List (intersperse)
import GHC.Generics
import Panini.Pretty.Printer
import Panini.Solver.Abstract.Lattice
import Prelude

-------------------------------------------------------------------------------

-- | An abstract character.
data AChar = AChar Bool IntSet
  deriving stock (Eq, Generic, Show, Read)

instance Hashable AChar

instance MeetSemilattice AChar where
  AChar True  xs ⊓ AChar True  ys = AChar True  (I.intersection xs ys)
  AChar False xs ⊓ AChar False ys = AChar False (I.union xs ys)
  AChar True  xs ⊓ AChar False ys = AChar True  (I.difference xs ys)
  AChar False xs ⊓ AChar True  ys = AChar True  (I.difference ys xs)

instance BoundedMeetSemilattice AChar where
  (⊤) = AChar False mempty

instance JoinSemilattice AChar where
  AChar True  xs ⊔ AChar True  ys = AChar True  (I.union xs ys)
  AChar False xs ⊔ AChar False ys = AChar False (I.intersection xs ys)
  AChar True  xs ⊔ AChar False ys = AChar False (I.difference ys xs)
  AChar False xs ⊔ AChar True  ys = AChar False (I.difference xs ys)

instance BoundedJoinSemilattice AChar where
  (⊥) = AChar True mempty

instance Complementable AChar where
  neg (AChar b xs) = AChar (not b) xs

instance ComplementedLattice AChar

-- | The number of concrete values represented by the abstract character.
concreteSize :: AChar -> Int
concreteSize (AChar True  cs) = I.size cs
concreteSize (AChar False cs) = fromEnum @Char maxBound - I.size cs

-- | The concrete values represented by the abstract character.
concreteValues :: AChar -> [Char]
concreteValues (AChar True  cs) = map (toEnum @Char) $ I.toAscList cs
concreteValues (AChar False cs) = 
  filter (\x -> fromEnum x `I.notMember` cs) $ enumFromTo minBound maxBound

-- | An abstract character @= c@.
aCharEq :: Char -> AChar
aCharEq = AChar True . I.singleton . fromEnum

-- | An abstract character @≠ c@, i.e., @= Σ\c@.
aCharNe :: Char -> AChar
aCharNe = AChar False . I.singleton . fromEnum

instance Pretty AChar where
  pretty (AChar True cs) = case map (toEnum @Char) $ I.toAscList cs of
    []  -> "∅"
    [x] -> "\"" <> pretty x <> "\""
    xs  -> "{" <> (mconcat $ intersperse "," $ map pretty xs) <> "}"
  
  pretty (AChar False cs) = case map (toEnum @Char) $ I.toAscList cs of
    []  -> "Σ"
    [x] -> "Σ∖" <> pretty x
    xs  -> "Σ∖{" <> (mconcat $ intersperse "," $ map pretty xs) <> "}"
