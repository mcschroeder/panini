{-# LANGUAGE StrictData #-}

module Panini.Solver.Abstract.AInteger
  ( AInteger
  , isInfinite
  , isEmpty
  , concreteInteger
  , aIntegerEq
  , aIntegerNe
  , aIntegerGt
  , aIntegerGe
  , aIntegerLt
  , aIntegerLe
  ) where

import Data.List (intersperse)
import Panini.Pretty.Printer
import Panini.Solver.Abstract.Lattice
import Prelude hiding (isInfinite)

-------------------------------------------------------------------------------

-- | An abstract integer.
newtype AInteger = AInteger IntervalSequence
  deriving stock (Eq, Show, Read)
  deriving newtype 
    ( MeetSemilattice, BoundedMeetSemilattice
    , JoinSemilattice, BoundedJoinSemilattice
    )

-- | Does an abstract integer represent an infinite number of values?
isInfinite :: AInteger -> Bool
isInfinite (AInteger (In NegInf _ : _))     = True
isInfinite (AInteger (last -> In _ PosInf)) = True
isInfinite _                                = False

-- | Does an abstract integer represent no values?
isEmpty :: AInteger -> Bool
isEmpty (AInteger []) = True
isEmpty _             = False

-- | The single concrete value represented by the abstract integer, or Nothing.
concreteInteger :: AInteger -> Maybe Integer
concreteInteger (AInteger [In (Fin a) (Fin b)]) | a == b = Just a
concreteInteger _                                        = Nothing

-- | An abstract integer @= i@.
aIntegerEq :: Integer -> AInteger
aIntegerEq a = AInteger [singleton a]

-- | An abstract integer @≠ i@, i.e., @{[-∞..i-1],[i+1..+∞]}@.
aIntegerNe :: Integer -> AInteger
aIntegerNe a = AInteger [In NegInf (Fin (a - 1)), In (Fin (a + 1)) PosInf]

-- | An abstract integer @> i@, i.e., @[i+1..+∞]@.
aIntegerGt :: Integer -> AInteger
aIntegerGt a = AInteger [In (Fin (a + 1)) PosInf]

-- | An abstract integer @≥ i@, i.e., @[i..+∞]@.
aIntegerGe :: Integer -> AInteger
aIntegerGe a = AInteger [In (Fin a) PosInf]

-- | An abstract integer @< i@, i.e., @[-∞..i-1]@.
aIntegerLt :: Integer -> AInteger
aIntegerLt a = AInteger [In NegInf (Fin (a - 1))]

-- | An abstract integer @≤ i@, i.e., @[-∞..i]@.
aIntegerLe :: Integer -> AInteger
aIntegerLe a = AInteger [In NegInf (Fin a)]

instance Pretty AInteger where
  pretty (AInteger [])  = "∅"
  pretty (AInteger [x]) = pretty x
  pretty (AInteger xs)  =
    "{" <> (mconcat $ intersperse "," $ map pretty xs) <> "}"

-------------------------------------------------------------------------------

-- | An ordered list of non-overlapping intervals.
type IntervalSequence = [Interval]

instance JoinSemilattice IntervalSequence where
  []     ⊔ ys        = ys
  xs     ⊔ []        = xs
  (x:xs) ⊔ (y:ys)
    | x `precedes` y = x : (xs ⊔ (y:ys))
    | y `precedes` x = y : ((x:xs) ⊔ ys)
    | otherwise      = ((x ⊔ y) : xs) ⊔ ys 

instance BoundedJoinSemilattice IntervalSequence where
  (⊥) = []

instance MeetSemilattice IntervalSequence where
  []     ⊓ _         = []
  _      ⊓ []        = []
  (x:xs) ⊓ (y:ys)
    | x `before` y   = xs ⊓ (y:ys)
    | y `before` x   = (x:xs) ⊓ ys
    | x `contains` y = (x ⊓ y) : ((x:xs) ⊓ ys)
    | y `contains` x = (x ⊓ y) : (xs ⊓ (y:ys))
    | otherwise      = (x ⊓ y) : (xs ⊓ ys)

instance BoundedMeetSemilattice IntervalSequence where
  (⊤) = [(⊤)]

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

instance JoinSemilattice Interval where
  In a b ⊔ In c d = In (min a c) (max b d)

instance MeetSemilattice Interval where
  In a b ⊓ In c d = In (max a c) (min b d)

instance BoundedMeetSemilattice Interval where
  (⊤) = In NegInf PosInf

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
