{-# LANGUAGE StrictData #-}

module Panini.Abstract.AInt
  ( AInt
  , pattern AInt0
  , pattern AInt1
  , count
  , values
  , intervals
  , member
  , minimum
  , maximum
  , continuous
  , finite
  , isLe
  , eq
  , ne
  , gt
  , ge
  , lt
  , le
  , gtA
  , geA
  , ltA
  , leA
  , add
  , sub
  , IntervalSequence
  , holes
  , Interval(..)
  , Inf(..)
  ) where

import Algebra.Lattice
import Data.Data (Data)
import Data.Hashable
import Data.List qualified as List
import Panini.Abstract.Interval
import Panini.Panic
import Panini.Pretty
import Prelude hiding (minimum, maximum)

-- TODO: investigate whether it's possible to have a Num instance

-------------------------------------------------------------------------------

-- | An abstract integer.
newtype AInt = AInt IntervalSequence
  deriving stock (Show, Read, Data)
  deriving newtype 
    ( Eq, Ord
    , PartialOrder
    , MeetSemilattice, JoinSemilattice
    , BoundedMeetSemilattice, BoundedJoinSemilattice
    , ComplementedLattice
    , Hashable)

instance Pretty AInt where
  pretty = ann (Literal AbstractLit) . \case
    AInt []  -> emptySet
    AInt [x] -> pretty x
    AInt xs  -> brackets $ mconcat $ List.intersperse mid 
              $ [pretty a <> comma <> pretty b | In a b <- xs]

pattern AInt0 :: AInt
pattern AInt0 <- (values -> [0]) where
  AInt0 = eq 0

pattern AInt1 :: AInt
pattern AInt1 <- (values -> [1]) where
  AInt1 = eq 1

-------------------------------------------------------------------------------

-- | The number of concrete values represented by the abstract integer (i.e.,
-- the length of the list returned by 'values'), or 'Nothing' if the
-- number of concrete values is infinite.
count :: AInt -> Maybe Integer
count (AInt xs) = go 0 xs
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
values :: AInt -> [Integer]
values (AInt xs) = go xs
  where
    go (In (Fin a) (Fin b) : ys) = [a..b] ++ go ys
    go (In (Fin a) PosInf  : _ ) = [a..]
    go (In NegInf  (Fin b) : ys) = interleave [b,b-1..] (go ys)
    go (In NegInf  PosInf  : _ ) = interleave [0..] [-1,-2..]
    go []                        = []
    go _                         = impossible

-- | Returns intervals of all integers represented by the abstract integer.
intervals :: AInt -> IntervalSequence
intervals (AInt xs) = xs

-- | Is the value represented by the abstract integer?
member :: Integer -> AInt -> Bool
member n (AInt xs)= go xs
  where
    go (In (Fin a) (Fin b) : ys) | n >= a, n <= b = True
                                 | otherwise      = go ys
    go (In (Fin a) PosInf  : _ ) | n >= a         = True
                                 | otherwise      = False
    go (In NegInf  (Fin b) : ys) | n <= b         = True
                                 | otherwise      = go ys
    go (In NegInf  PosInf  : _ ) = True
    go []                        = False
    go _                         = impossible

-- | Interleaves two lists.
interleave :: [a] -> [a] -> [a]
interleave (x:xs) ys = x : interleave ys xs
interleave []     ys = ys

-- | The smallest value represented by the abstract integer, which might be -∞
-- or +∞, or 'Nothing' if the integer is ⊥.
minimum :: AInt -> Maybe (Inf Integer)
minimum (AInt xs) = case xs of
  []         -> Nothing
  In a _ : _ -> Just a

-- | The largest value represented by the abstract integer, which might +∞ or
-- -∞, or 'Nothing' if the integer is ⊥.
maximum :: AInt -> Maybe (Inf Integer)
maximum (AInt xs) = case xs of
  []               -> Nothing
  (last -> In _ b) -> Just b

-- | Whether the abstract integer is continuous, i.e., whether it contains all
-- integers between its `minimum` and `maximum` values.
continuous :: AInt -> Bool
continuous (AInt xs) = case xs of
  []  -> True
  [_] -> True
  _   -> False

-- | Whether the abstract integer represents a finite number of values.
finite :: AInt -> Bool
finite (AInt xs) = case xs of
  (In NegInf _):_ -> False
  (last -> In _ PosInf) -> False
  _ -> True

-- | Whether the abstract integer solely represents values less than or equal to
-- a given integer.
isLe :: AInt -> Integer -> Bool
isLe a b = case maximum a of
  Just NegInf  -> True
  Just (Fin n) -> n <= b
  _            -> False

-------------------------------------------------------------------------------

-- | An abstract integer @= i@.
eq :: Integer -> AInt
eq a = AInt [singleton a]

-- | An abstract integer @≠ i@, i.e., @{[-∞..i-1],[i+1..+∞]}@.
ne :: Integer -> AInt
ne a = AInt [In NegInf (Fin (a - 1)), In (Fin (a + 1)) PosInf]

-- | An abstract integer @> i@, i.e., @[i+1..+∞]@.
gt :: Integer -> AInt
gt a = AInt [In (Fin (a + 1)) PosInf]

-- | An abstract integer @≥ i@, i.e., @[i..+∞]@.
ge :: Integer -> AInt
ge a = AInt [In (Fin a) PosInf]

-- | An abstract integer @< i@, i.e., @[-∞..i-1]@.
lt :: Integer -> AInt
lt a = AInt [In NegInf (Fin (a - 1))]

-- | An abstract integer @≤ i@, i.e., @[-∞..i]@.
le :: Integer -> AInt
le a = AInt [In NegInf (Fin a)]

-- | An abstract integer @> α@, i.e., @[(minimum α + 1)..+∞]@.
gtA :: AInt -> AInt
gtA (AInt xs) = case xs of
  []         -> AInt []
  In a _ : _ -> AInt [In (succ <$> a) PosInf]

-- | An abstract integer @≥ α@, i.e., @[(minimum α)..+∞]@.
geA :: AInt -> AInt
geA (AInt xs) = case xs of
  []         -> AInt []
  In a _ : _ -> AInt [In a PosInf]

-- | An abstract integer @< α@, i.e., @[-∞..(maximum α - 1)]@.
ltA :: AInt -> AInt
ltA (AInt xs) = case xs of
  []               -> AInt []
  (last -> In _ a) -> AInt [In NegInf (pred <$> a)]

-- | An abstract integer @≤ α@, i.e., @[-∞..(maximum α)]@.
leA :: AInt -> AInt
leA (AInt xs) = case xs of
  []               -> AInt []
  (last -> In _ a) -> AInt [In NegInf a]

-- | Add two abstract integers together, i.e., form the union of adding every
-- value represented by the first abstract integer to every value represented by
-- the second abstract integer.
add :: AInt -> AInt -> AInt
add (AInt xs) (AInt ys) = AInt $ joins $ [[addIn x y] | x <- xs, y <- ys]

-- | Subtract one abstract integer from another, i.e., form the union of
-- subtracting every value represented by the second abstract integer from every
-- value represented by the first abstract integer.
sub :: AInt -> AInt -> AInt
sub (AInt xs) (AInt ys) = AInt $ joins $ [[subIn x y] | x <- xs, y <- ys]
