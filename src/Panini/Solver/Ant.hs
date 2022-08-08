{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Panini.Solver.Ant
  ( Ant
  , isInfinite
  , isEmpty
  , concreteValue
  , mkAntEq, mkAntNeq, mkAntGt, mkAntGeq, mkAntLt, mkAntLeq
  , join
  , meet
  ) where

import Panini.Printer
import Prelude hiding (isInfinite)
import Data.List (intersperse)

-------------------------------------------------------------------------------

-- | An abstract integer.
data Ant 
  = Empty
  | Concrete Integer
  | Abstract [Interval]

isInfinite :: Ant -> Bool
isInfinite (Abstract (In NegInf _ : _)) = True
isInfinite (Abstract (last -> In _ PosInf)) = True
isInfinite _ = False

isEmpty :: Ant -> Bool
isEmpty Empty = True
isEmpty _     = False

concreteValue :: Ant -> Maybe Integer
concreteValue (Concrete n) = Just n
concreteValue _            = Nothing

mkAntEq :: Integer -> Ant
mkAntEq a = Concrete a

mkAntNeq :: Integer -> Ant
mkAntNeq a = Abstract [In NegInf (Fin (a - 1)), In (Fin (a + 1)) PosInf]

mkAntGt :: Integer -> Ant
mkAntGt a = Abstract [In (Fin (a + 1)) PosInf]

mkAntGeq :: Integer -> Ant
mkAntGeq a = Abstract [In (Fin a) PosInf]

mkAntLt :: Integer -> Ant
mkAntLt a = Abstract [In NegInf (Fin (a - 1))]

mkAntLeq :: Integer -> Ant
mkAntLeq a = Abstract [In NegInf (Fin a)]

-- | Join two abstract integers (widening).
join :: Ant -> Ant -> Ant
join Empty b = b
join a Empty = a
join (Abstract xs) (Abstract ys) = Abstract $ joinIntervals xs ys
join (Abstract xs) (Concrete a) = Abstract $ joinIntervals [In (Fin a) (Fin a)] xs
join (Concrete a) (Abstract xs) = Abstract $ joinIntervals [In (Fin a) (Fin a)] xs
join (Concrete a) (Concrete b)
  | a < b = Abstract [In (Fin a) (Fin a), In (Fin b) (Fin b)]
  | a > b = Abstract [In (Fin b) (Fin b), In (Fin a) (Fin a)]
  | otherwise = Concrete a

-- | Meet two abstract integers (narrowing).
meet :: Ant -> Ant -> Ant
meet Empty _ = Empty
meet _ Empty = Empty
meet (Abstract xs) (Abstract ys) = 
  let zs = meetIntervals xs ys
  in if null zs then Empty else Abstract zs
meet (Abstract xs) (Concrete a) = 
  let zs = meetIntervals [In (Fin a) (Fin a)] xs
  in if null zs then Empty else Abstract zs
meet (Concrete a) (Abstract xs) =
  let zs = meetIntervals [In (Fin a) (Fin a)] xs
  in if null zs then Empty else Abstract zs
meet (Concrete a) (Concrete b)
  | a == b = Concrete a
  | otherwise = Empty

instance Pretty Ant where
  pretty Empty = "∅"
  pretty (Concrete a) = pretty a
  pretty (Abstract xs) = "{" <> (mconcat $ intersperse "," $ map pretty xs) <> "}"

-------------------------------------------------------------------------------

-- | An integer interval [a..b] where a <= b.
data Interval = In (Inf Integer) (Inf Integer)
  deriving stock (Eq, Show, Read)

-- | Merge two non-overlapping ordered lists of intervals using join semantics.
joinIntervals :: [Interval] -> [Interval] -> [Interval]
joinIntervals []     = id
joinIntervals (x:xs) = joinIntervals xs . go x
  where
    go (In a b) [] = [In a b]
    go (In a b) (In c d : ys)
      | b < c     = In a b : In c d : ys
      | a > d     = In c d : go (In a b) ys
      | otherwise = In (min a c) (max b d) : ys

-- | Merge two non-overlapping ordered lists of intervals using meet semantics.
meetIntervals :: [Interval] -> [Interval] -> [Interval]
meetIntervals [] = id
meetIntervals (x:xs) = meetIntervals xs . go x
  where
    go _ [] = []
    go (In a b) (In c d : ys)
      | b < c     = []
      | a > d     = go (In a b) ys
      | otherwise = In (max a c) (min b d) : ys

instance Pretty Interval where
  pretty (In a b)
    | a == b    = pretty a
    | otherwise = "[" <> pretty a <> ".." <> pretty b <> "]"

-------------------------------------------------------------------------------

-- | A type extended with negative and positive infinity.
data Inf a = NegInf | Fin a | PosInf
  deriving stock (Eq, Show, Read)

instance Ord a => Ord (Inf a) where
  compare NegInf NegInf   = EQ
  compare NegInf _        = LT
  compare (Fin _) NegInf  = GT
  compare (Fin a) (Fin b) = compare a b
  compare (Fin _) PosInf  = LT
  compare PosInf PosInf   = EQ
  compare PosInf _        = GT

instance Pretty a => Pretty (Inf a) where
  pretty NegInf = "-∞"
  pretty PosInf = "+∞"
  pretty (Fin a) = pretty a
