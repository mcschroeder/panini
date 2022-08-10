{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Panini.Solver.AbstractInteger
  ( AbstractInteger
  , isInfinite
  , isEmpty
  , concreteValue
  , mkEq, mkNeq, mkGt, mkGeq, mkLt, mkLeq
  , join
  , meet
  ) where

import Panini.Printer
import Prelude hiding (isInfinite)
import Data.List (intersperse)

-------------------------------------------------------------------------------

-- | An abstract integer.
data AbstractInteger 
  = Empty
  | Concrete Integer
  | Abstract IntervalSequence
  deriving stock (Show, Read)

isInfinite :: AbstractInteger -> Bool
isInfinite (Abstract (In NegInf _ : _)) = True
isInfinite (Abstract (last -> In _ PosInf)) = True
isInfinite _ = False

isEmpty :: AbstractInteger -> Bool
isEmpty Empty = True
isEmpty _     = False

concreteValue :: AbstractInteger -> Maybe Integer
concreteValue (Concrete n) = Just n
concreteValue _            = Nothing

mkEq :: Integer -> AbstractInteger
mkEq a = Concrete a

mkNeq :: Integer -> AbstractInteger
mkNeq a = Abstract [In NegInf (Fin (a - 1)), In (Fin (a + 1)) PosInf]

mkGt :: Integer -> AbstractInteger
mkGt a = Abstract [In (Fin (a + 1)) PosInf]

mkGeq :: Integer -> AbstractInteger
mkGeq a = Abstract [In (Fin a) PosInf]

mkLt :: Integer -> AbstractInteger
mkLt a = Abstract [In NegInf (Fin (a - 1))]

mkLeq :: Integer -> AbstractInteger
mkLeq a = Abstract [In NegInf (Fin a)]

-- | Join two abstract integers (widening).
join :: AbstractInteger -> AbstractInteger -> AbstractInteger
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
meet :: AbstractInteger -> AbstractInteger -> AbstractInteger
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

instance Pretty AbstractInteger where
  pretty Empty = "∅"
  pretty (Concrete a) = pretty a
  pretty (Abstract [x]) = pretty x
  pretty (Abstract xs) = "{" <> (mconcat $ intersperse "," $ map pretty xs) <> "}"

-------------------------------------------------------------------------------

-- | An integer interval [a..b] where a <= b.
data Interval = In (Inf Integer) (Inf Integer)
  deriving stock (Eq, Show, Read)

-- | An ordered list of non-overlapping intervals.
type IntervalSequence = [Interval]

-- | Merge two lists of intervals using join semantics.
joinIntervals :: IntervalSequence -> IntervalSequence -> IntervalSequence
joinIntervals []     = id
joinIntervals (x:xs) = joinIntervals xs . go x
  where
    go (In a b) [] = [In a b]    
    go (In a b) (In c d : ys)
      | b < (pred <$> c) = In a b : In c d : ys
      | a > (succ <$> d) = In c d : go (In a b) ys
      | otherwise        = In (min a c) (max b d) : ys

-- | Merge two lists of intervals using meet semantics.
meetIntervals :: IntervalSequence -> IntervalSequence -> IntervalSequence
meetIntervals xs0 ys0 = go [] xs0 ys0
  where    
    go zs [] _ = reverse zs
    go zs _ [] = reverse zs
    go zs (In a b : xs) (In c d : ys)
      | b < c     = go                           zs            xs (In c d : ys)
      | a > d     = go                           zs  (In a b : xs)          ys
      | b > d     = go (In (max a c) (min b d) : zs) (In a b : xs)          ys
      | d > b     = go (In (max a c) (min b d) : zs)           xs (In c d : ys)
      | otherwise = go (In (max a c) (min b d) : zs)           xs           ys

instance Pretty Interval where
  pretty (In a b)
    | a == b    = pretty a
    | otherwise = "[" <> pretty a <> ".." <> pretty b <> "]"

-------------------------------------------------------------------------------

-- | A type extended with negative and positive infinity.
data Inf a = NegInf | Fin a | PosInf
  deriving stock (Eq, Functor, Show, Read)

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
