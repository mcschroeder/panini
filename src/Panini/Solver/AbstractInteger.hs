{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Panini.Solver.AbstractInteger
  ( AInteger
  , isInfinite
  , isEmpty
  , concreteValue
  , mkEq, mkNeq, mkGt, mkGeq, mkLt, mkLeq
  ) where

import Algebra.Lattice
import Panini.Printer
import Prelude hiding (isInfinite)
import Data.List (intersperse)

-------------------------------------------------------------------------------

-- | An abstract integer.
newtype AInteger = AInteger IntervalSequence
  deriving stock (Eq, Show, Read)
  deriving newtype (Lattice, BoundedJoinSemiLattice, BoundedMeetSemiLattice)

isInfinite :: AInteger -> Bool
isInfinite (AInteger (In NegInf _ : _))     = True
isInfinite (AInteger (last -> In _ PosInf)) = True
isInfinite _                                = False

isEmpty :: AInteger -> Bool
isEmpty = (== bottom)

concreteValue :: AInteger -> Maybe Integer
concreteValue (AInteger [In (Fin a) (Fin b)]) | a == b = Just a
concreteValue _                                        = Nothing

mkEq :: Integer -> AInteger
mkEq a = AInteger [singleton a]

mkNeq :: Integer -> AInteger
mkNeq a = AInteger [In NegInf (Fin (a - 1)), In (Fin (a + 1)) PosInf]

mkGt :: Integer -> AInteger
mkGt a = AInteger [In (Fin (a + 1)) PosInf]

mkGeq :: Integer -> AInteger
mkGeq a = AInteger [In (Fin a) PosInf]

mkLt :: Integer -> AInteger
mkLt a = AInteger [In NegInf (Fin (a - 1))]

mkLeq :: Integer -> AInteger
mkLeq a = AInteger [In NegInf (Fin a)]

instance Pretty AInteger where
  pretty (AInteger [])  = "∅"
  pretty (AInteger [x]) = pretty x
  pretty (AInteger xs)  =
    "{" <> (mconcat $ intersperse "," $ map pretty xs) <> "}"

-------------------------------------------------------------------------------

-- | An ordered list of non-overlapping intervals.
type IntervalSequence = [Interval]

instance Lattice IntervalSequence where
  []     \/ ys       = ys
  xs     \/ []       = xs
  (x:xs) \/ (y:ys)
    | x `precedes` y = x : (xs \/ (y:ys))
    | y `precedes` x = y : ((x:xs) \/ ys)
    | otherwise      = ((x \/ y) : xs) \/ ys 

  []     /\ _        = []
  _      /\ []       = []
  (x:xs) /\ (y:ys)
    | x `before` y   = xs /\ (y:ys)
    | y `before` x   = (x:xs) /\ ys
    | x `contains` y = (x /\ y) : ((x:xs) /\ ys)
    | y `contains` x = (x /\ y) : (xs /\ (y:ys))
    | otherwise      = (x /\ y) : (xs /\ ys)

instance BoundedJoinSemiLattice IntervalSequence where
  bottom = []

instance BoundedMeetSemiLattice IntervalSequence where
  top = [top]

-------------------------------------------------------------------------------

-- | An integer interval @[a..b]@ where @a <= b@.
data Interval = In (Inf Integer) (Inf Integer)
  deriving stock (Eq, Show, Read)

-- | Create a singleton interval @[a..a]@.
singleton :: Integer -> Interval
singleton a = In (Fin a) (Fin a)

-- | @[a..b]@ precedes @[c..d]@ if @b < (c - 1)@.
--
-- This is precisely the "precedes" relation from Allen's interval algebra.
precedes :: Interval -> Interval -> Bool
precedes (In _ b) (In c _) = b < (pred <$> c)

-- | @[a..b]@ is before @[c..d]@ if @b < c@.
--
-- In terms of Allen's interval algebra, this would be "precedes or meets".
before :: Interval -> Interval -> Bool
before (In _ b) (In c _) = b < c

-- | @[a..b]@ contains @[c..d]@ if @a <= c@ and @d <= b@.
--
-- This is not the "contains" relation from Allen's interval algebra, 
-- but is equivalent to "contains or equals or starts or is finished by".
contains :: Interval -> Interval -> Bool
contains (In a b) (In c d) = a <= c && d <= b

instance Lattice Interval where
  In a b \/ In c d = In (min a c) (max b d)
  In a b /\ In c d = In (max a c) (min b d)

instance BoundedMeetSemiLattice Interval where
  top = In NegInf PosInf

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
