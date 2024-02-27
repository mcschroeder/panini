{-# LANGUAGE StrictData #-}

module Panini.Abstract.AInt
  ( AInt
  , count
  , values
  , intervals
  , member
  , minimum
  , maximum
  , continuous
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
import Data.Hashable
import GHC.Generics
import Panini.Panic
import Panini.Pretty
import Prelude hiding (minimum, maximum)
import Prettyprinter qualified as PP

-------------------------------------------------------------------------------

-- | An abstract integer.
newtype AInt = AInt IntervalSequence
  deriving stock (Show, Read)
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
    AInt xs  -> PP.encloseSep lbracket rbracket mid 
              $ map (\(In a b) -> pretty a <> comma <> pretty b) xs

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

-------------------------------------------------------------------------------

-- | An ordered list of non-overlapping integer intervals.
type IntervalSequence = [Interval]

instance {-# OVERLAPPING #-} Ord IntervalSequence where
  (last -> In _ a) <= (last -> In _ b) = a <= b

instance PartialOrder IntervalSequence where
  (⊑) = (<=)

instance JoinSemilattice IntervalSequence where
  []     ∨ ys        = ys
  xs     ∨ []        = xs
  (x:xs) ∨ (y:ys)
    | x `precedes` y = x : (xs ∨ (y:ys))
    | y `precedes` x = y : ((x:xs) ∨ ys)
    | otherwise      = ([x ∨ y] ∨ xs) ∨ ys

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

instance ComplementedLattice IntervalSequence where
  neg [] = [top]  
  neg (x:xs)
    | In a@(Fin _) _ <- x = In NegInf (pred <$> a) : go (x:xs)
    | otherwise = go (x:xs)
    where
      go (In _ b : y@(In c _) : zs) = In (succ <$> b) (pred <$> c) : go (y:zs)
      go [In _ b@(Fin _)] = [In (succ <$> b) PosInf]
      go _ = []  

-- | Returns all "holes" inbetween intervals.
holes :: IntervalSequence -> [Integer]
holes (In _ (Fin b) : y@(In (Fin c) _) : ys) = [b + 1 .. c - 1] ++ holes (y:ys)
holes _                                      = []

-------------------------------------------------------------------------------

-- | An integer interval @[a..b]@ where @a <= b@.
data Interval = In !(Inf Integer) !(Inf Integer)
  deriving stock (Eq, Generic, Show, Read)

instance Hashable Interval

-- | Uses 'PartialOrder' instance.
instance Ord Interval where
  (<=) = (⊑)

-- | Create a singleton interval @[a..a]@.
singleton :: Integer -> Interval
singleton a = In (Fin a) (Fin a)

-- | Add two intervals together, i.e., @[a,b] + [c,d] = [a+b,c+d]@.
addIn :: Interval -> Interval -> Interval
addIn = curry $ \case
  (In (Fin a) (Fin b), In (Fin c) (Fin d)) -> In (Fin $ a + c) (Fin $ b + d)
  (In (Fin a) PosInf , In (Fin c) _      ) -> In (Fin $ a + c) PosInf
  (In (Fin a) _      , In (Fin c) PosInf ) -> In (Fin $ a + c) PosInf
  (In NegInf  (Fin b), In _       (Fin d)) -> In NegInf        (Fin $ b + d)
  (In _       (Fin b), In NegInf  (Fin d)) -> In NegInf        (Fin $ b + d)
  (In NegInf  _      , In _       PosInf ) -> In NegInf        PosInf
  (In _       PosInf , In NegInf  _      ) -> In NegInf        PosInf
  (In _       _      , In NegInf  PosInf ) -> In NegInf        PosInf
  (In NegInf  PosInf , In _       _      ) -> In NegInf        PosInf  
  _                                        -> impossible

-- | Subtract two intervals, i.e., @[a,b] - [c,d] = [a-d,b-c]@.
subIn :: Interval -> Interval -> Interval
subIn = curry $ \case
  (In (Fin a) (Fin b), In (Fin c) (Fin d)) -> In (Fin $ a - d) (Fin $ b - c)
  (In (Fin a) PosInf , In _       (Fin d)) -> In (Fin $ a - d) PosInf
  (In _       (Fin b), In (Fin c) PosInf ) -> In NegInf        (Fin $ b - c)
  (In NegInf  (Fin b), In (Fin c) _      ) -> In NegInf        (Fin $ b - c)
  (In (Fin a) _      , In NegInf  (Fin d)) -> In (Fin $ a - d) PosInf
  (In _       _      , In NegInf  PosInf ) -> In NegInf        PosInf
  (In NegInf  PosInf , In _       _      ) -> In NegInf        PosInf
  (In NegInf  _      , In NegInf  _      ) -> In NegInf        PosInf
  (In _       PosInf , In _       PosInf ) -> In NegInf        PosInf  
  _                                        -> impossible

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

-- | Intervals are ordered by inclusion.
instance PartialOrder Interval where
  In a b ⊑ In c d = c <= a && b <= d

instance JoinSemilattice Interval where
  In a b ∨ In c d = In (min a c) (max b d)

instance MeetSemilattice Interval where
  In a b ∧ In c d = In (max a c) (min b d)

instance BoundedMeetSemilattice Interval where
  top = In NegInf PosInf

instance Pretty Interval where
  pretty (In a b)
    | a == b    = pretty a
    | otherwise = brackets $ pretty a <> comma <> pretty b

-------------------------------------------------------------------------------

-- | A type extended with negative and positive infinity.
data Inf a = NegInf | Fin !a | PosInf
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
  pretty NegInf = "-" <> symInf
  pretty PosInf = "+" <> symInf
  pretty (Fin a) = pretty a
