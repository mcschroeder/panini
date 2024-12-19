module Panini.Abstract.Interval where

import Algebra.Lattice
import Data.Data (Data)
import Data.Hashable
import GHC.Generics
import Panini.Panic
import Panini.Pretty
import Prelude hiding (minimum, maximum)

-------------------------------------------------------------------------------

-- | A type extended with negative and positive infinity.
data Inf a = NegInf | Fin !a | PosInf
  deriving stock (Functor, Eq, Generic, Show, Read, Data)

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

-------------------------------------------------------------------------------

-- | An integer interval @[a..b]@ where @a <= b@.
data Interval = In !(Inf Integer) !(Inf Integer)
  deriving stock (Eq, Generic, Show, Read, Data)

instance Hashable Interval

-- | A linear extension of the 'PartialOrder'.
instance Ord Interval where
  a <= b = a ⊑ b || a `before` b || a `overlaps` b

-- | Intervals are partially ordered by inclusion.
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

-- | Infix pattern synonym for the 'In' constructor.
pattern (:…) :: Inf Integer -> Inf Integer -> Interval
pattern a :… b = In a b

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

-------------------------------------------------------------------------------

-- | An ordered list of non-overlapping integer intervals.
type IntervalSequence = [Interval]

-- | A linear extension of the 'PartialOrder'.
instance {-# OVERLAPPING #-} Ord IntervalSequence where
  a <= b | a ⊑ b     = True
         | b ⊑ a     = False
         | otherwise = head a <= head b

-- | Interval sequences are partially ordered by inclusion.
instance PartialOrder IntervalSequence where
  []     ⊑ _       = True
  _      ⊑ []      = False
  (x:xs) ⊑ (y:ys)
    | x `before` y = False
    | y `before` x = (x:xs) ⊑ ys
    | x ⊑ y        = xs ⊑ (y:ys)
    | otherwise    = False

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
