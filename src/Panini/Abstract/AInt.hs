{-# LANGUAGE StrictData #-}

module Panini.Abstract.AInt
  ( AInt
  , concreteSize
  , concreteValues
  , aMinimum
  , aMaximum
  -- , toPred
  , aIntegerEq
  , aIntegerNe
  , aIntegerGt
  , aIntegerGe
  , aIntegerLt
  , aIntegerLe
  , aIntegerGtA
  , aIntegerGeA
  , aIntegerLtA
  , aIntegerLeA
  , aIntegerAddI
  , aContinuous
  , Inf(..)
  ) where

import Data.Hashable
import GHC.Generics
import Panini.Algebra.Lattice
import Panini.Pretty.Printer
-- import Panini.Syntax
import Prelude
import Prettyprinter qualified as PP

-------------------------------------------------------------------------------

-- | An abstract integer.
newtype AInt = AInt IntervalSequence
  deriving stock (Eq, Show, Read)
  deriving newtype 
    ( MeetSemilattice, BoundedMeetSemilattice
    , JoinSemilattice, BoundedJoinSemilattice
    , Complementable
    , Hashable
    )

-- | The number of concrete values represented by the abstract integer (i.e.,
-- the length of the list returned by 'concreteValues'), or 'Nothing' if the
-- number of concrete values is infinite.
concreteSize :: AInt -> Maybe Integer
concreteSize (AInt xs) = go 0 xs
  where
    go n (In (Fin a) (Fin b) : ys) = go (n + 1 + b - a) ys
    go n []                        = Just n
    go _ _                         = Nothing

-- | The concrete values represented by the abstract integer. 
--
-- Note that this is potentially an infinite list. If the number of values is
-- finite, or if they approach only positive infinity (+∞), the values are
-- returned in ascending order. If the values (also) tend toward negative
-- infinity (-∞), no ordering guarantees are given.
concreteValues :: AInt -> [Integer]
concreteValues (AInt xs) = go xs
  where
    go (In (Fin a) (Fin b) : ys) = [a..b] ++ go ys
    go (In (Fin a) PosInf  : _ ) = [a..]
    go (In NegInf  (Fin b) : ys) = interleave [b,b-1..] (go ys)
    go (In NegInf  PosInf  : _ ) = interleave [0..] [-1,-2..]
    go []                        = []
    go _                         = error "impossible"

-- | Interleaves two lists.
interleave :: [a] -> [a] -> [a]
interleave (x:xs) ys = x : interleave ys xs
interleave []     ys = ys

-- | The smallest value represented by the abstract integer, which might be -∞
-- or +∞, or 'Nothing' if the integer is ⊥.
aMinimum :: AInt -> Maybe (Inf Integer)
aMinimum (AInt xs) = case xs of
  []         -> Nothing
  In a _ : _ -> Just a

-- | The largest value represented by the abstract integer, which might +∞ or
-- -∞, or 'Nothing' if the integer is ⊥.
aMaximum :: AInt -> Maybe (Inf Integer)
aMaximum (AInt xs) = case xs of
  []               -> Nothing
  (last -> In _ b) -> Just b

aContinuous :: AInt -> Bool
aContinuous (AInt xs) = case xs of
  []  -> True
  [_] -> True
  _   -> False

-- | An abstract integer @= i@.
aIntegerEq :: Integer -> AInt
aIntegerEq a = AInt [singleton a]

-- | An abstract integer @≠ i@, i.e., @{[-∞..i-1],[i+1..+∞]}@.
aIntegerNe :: Integer -> AInt
aIntegerNe a = AInt [In NegInf (Fin (a - 1)), In (Fin (a + 1)) PosInf]

-- | An abstract integer @> i@, i.e., @[i+1..+∞]@.
aIntegerGt :: Integer -> AInt
aIntegerGt a = AInt [In (Fin (a + 1)) PosInf]

-- | An abstract integer @≥ i@, i.e., @[i..+∞]@.
aIntegerGe :: Integer -> AInt
aIntegerGe a = AInt [In (Fin a) PosInf]

-- | An abstract integer @< i@, i.e., @[-∞..i-1]@.
aIntegerLt :: Integer -> AInt
aIntegerLt a = AInt [In NegInf (Fin (a - 1))]

-- | An abstract integer @≤ i@, i.e., @[-∞..i]@.
aIntegerLe :: Integer -> AInt
aIntegerLe a = AInt [In NegInf (Fin a)]

-- TODO: document
aIntegerGtA :: AInt -> AInt
aIntegerGtA (AInt xs) = case xs of
  []               -> AInt []
  (last -> In _ b) -> AInt [In (succ <$> b) PosInf]

-- TODO: document
aIntegerGeA :: AInt -> AInt
aIntegerGeA (AInt xs) = case xs of
  []               -> AInt []
  (last -> In _ b) -> AInt [In b PosInf]

-- TODO: document
aIntegerLtA :: AInt -> AInt
aIntegerLtA (AInt xs) = case xs of
  []         -> AInt []
  In a _ : _ -> AInt [In NegInf (pred <$> a)]

-- TODO: document
aIntegerLeA :: AInt -> AInt
aIntegerLeA (AInt xs) = case xs of
  []         -> AInt []
  In a _ : _ -> AInt [In NegInf a]

-- toPred :: PExpr -> AInt -> Pred
-- toPred lhs (AInt xs) = case xs of
--   []                                          -> PFalse
--   In NegInf  PosInf  : []                     -> PTrue
--   In (Fin a) (Fin b) : [] | a == b            -> mkRel Eq a
--   In (Fin a) (Fin b) : []                     -> mkRel Ge a ∧ mkRel Le b
--   In NegInf  (Fin b) : []                     -> mkRel Le b
--   In (Fin a) PosInf  : []                     -> mkRel Ge a
--   In NegInf  _       : (last -> In _ (Fin b)) -> meets $ mkRel Le b : holeRels
--   In (Fin a) _       : (last -> In _ PosInf ) -> meets $ mkRel Ge a : holeRels
--   In NegInf  _       : (last -> In _ PosInf ) -> meets $ holeRels
--   _                                           -> error "impossible"
--  where
--   mkRel r i = PRel r lhs (PCon (I i NoPV))
--   holeRels  = map (mkRel Ne) (holes xs)

instance Pretty AInt where
  pretty (AInt [])  = "∅"
  pretty (AInt [x]) = pretty x
  pretty (AInt xs)  = PP.encloseSep lbracket rbracket symMid 
                    $ map (\(In a b) -> pretty a <> symComma <> pretty b) xs



aIntegerAddI :: AInt -> Integer -> AInt
aIntegerAddI (AInt xs) i = case xs of
  [In (Fin a) PosInf] -> AInt [In (Fin (a + i)) PosInf]
  _ -> undefined -- TODO

-------------------------------------------------------------------------------

-- | An ordered list of non-overlapping integer intervals.
type IntervalSequence = [Interval]

instance JoinSemilattice IntervalSequence where
  []     ∨ ys        = ys
  xs     ∨ []        = xs
  (x:xs) ∨ (y:ys)
    | x `precedes` y = x : (xs ∨ (y:ys))
    | y `precedes` x = y : ((x:xs) ∨ ys)
    | otherwise      = ((x ∨ y) : xs) ∨ ys 

instance BoundedJoinSemilattice IntervalSequence where
  bot = []

instance MeetSemilattice IntervalSequence where
  []     ∧ _         = []
  _      ∧ []        = []
  (x:xs) ∧ (y:ys)
    | x `before` y   = xs ∧ (y:ys)
    | y `before` x   = (x:xs) ∧ ys
    | x `contains` y = (x ∧ y) : ((x:xs) ∧ ys)
    | y `contains` x = (x ∧ y) : (xs ∧ (y:ys))
    | x `overlaps` y = (x ∧ y) : (xs ∧ (y:ys))
    | y `overlaps` x = (x ∧ y) : ((x:xs) ∧ ys)
    | otherwise      = (x ∧ y) : (xs ∧ ys)

instance BoundedMeetSemilattice IntervalSequence where
  top = [top]

instance Complementable IntervalSequence where
  neg [] = [top]  
  neg (x:xs)
    | In a@(Fin _) _ <- x = In NegInf (pred <$> a) : go (x:xs)
    | otherwise = go (x:xs)
    where
      go (In _ b : y@(In c _) : zs) = In (succ <$> b) (pred <$> c) : go (y:zs)
      go [In _ b@(Fin _)] = [In (succ <$> b) PosInf]
      go _ = []  

-- | Returns all "holes" inbetween intervals.
-- holes :: IntervalSequence -> [Integer]
-- holes (In _ (Fin b) : y@(In (Fin c) _) : ys) = [b + 1 .. c - 1] ++ holes (y:ys)
-- holes _                                      = []

-------------------------------------------------------------------------------

-- | An integer interval @[a..b]@ where @a <= b@.
data Interval = In (Inf Integer) (Inf Integer)
  deriving stock (Eq, Generic, Show, Read)

instance Hashable Interval

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
-- This is not the "contains" relation from Allen's interval algebra, but is
-- equivalent to "contains or equals or is started by or is finished by".
contains :: Interval -> Interval -> Bool
contains (In a b) (In c d) = a <= c && d <= b

-- | @[a..b]@ overlaps @[c..d]@ if @a <= c@ and @c <= b@ and @b < d@.
--
-- This is the "overlaps" relation in Allen's interval algebra.
overlaps :: Interval -> Interval -> Bool
overlaps (In a b) (In c d) = a <= c && c <= b && b < d

instance JoinSemilattice Interval where
  In a b ∨ In c d = In (min a c) (max b d)

instance MeetSemilattice Interval where
  In a b ∧ In c d = In (max a c) (min b d)

instance BoundedMeetSemilattice Interval where
  top = In NegInf PosInf

instance Pretty Interval where
  pretty (In a b)
    | a == b    = pretty a
    | otherwise = brackets $ pretty a <> symComma <> pretty b

-------------------------------------------------------------------------------

-- | A type extended with negative and positive infinity.
data Inf a = NegInf | Fin a | PosInf
  deriving stock (Functor, Eq, Generic, Show, Read)

instance Hashable a => Hashable (Inf a)

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
