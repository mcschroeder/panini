{-# LANGUAGE StrictData #-}

module Panini.Abstract.AInt
  ( AInt
  , concreteCount
  , concreteValues
  , concreteMember
  , minimum
  , maximum
  -- , toPred
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
  , addI
  , continuous
  , Inf(..)
  ) where

import Algebra.Lattice
import Data.Hashable
import GHC.Generics
import Panini.Pretty
-- import Panini.Syntax
import Prelude hiding (minimum, maximum)
import Prettyprinter qualified as PP

-------------------------------------------------------------------------------

-- | An abstract integer.
newtype AInt = AInt IntervalSequence
  deriving stock (Eq, Show, Read)
  deriving newtype 
    ( MeetSemilattice, JoinSemilattice
    , BoundedMeetSemilattice, BoundedJoinSemilattice
    , ComplementedLattice
    , Hashable)

-- | The number of concrete values represented by the abstract integer (i.e.,
-- the length of the list returned by 'concreteValues'), or 'Nothing' if the
-- number of concrete values is infinite.
concreteCount :: AInt -> Maybe Integer
concreteCount (AInt xs) = go 0 xs
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

-- | Is the value represented by the abstract integer?
concreteMember :: Integer -> AInt -> Bool
concreteMember n (AInt xs)= go xs
  where
    go (In (Fin a) (Fin b) : ys) | n >= a, n <= b = True
                                 | otherwise      = go ys
    go (In (Fin a) PosInf  : _ ) | n >= a         = True
                                 | otherwise      = False
    go (In NegInf  (Fin b) : ys) | n <= b         = True
                                 | otherwise      = go ys
    go (In NegInf  PosInf  : _ ) = True
    go []                        = False
    go _                         = error "impossible"

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

continuous :: AInt -> Bool
continuous (AInt xs) = case xs of
  []  -> True
  [_] -> True
  _   -> False

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

-- TODO: document
gtA :: AInt -> AInt
gtA (AInt xs) = case xs of
  []               -> AInt []
  (last -> In _ b) -> AInt [In (succ <$> b) PosInf]

-- TODO: document
geA :: AInt -> AInt
geA (AInt xs) = case xs of
  []               -> AInt []
  (last -> In _ b) -> AInt [In b PosInf]

-- TODO: document
ltA :: AInt -> AInt
ltA (AInt xs) = case xs of
  []         -> AInt []
  In a _ : _ -> AInt [In NegInf (pred <$> a)]

-- TODO: document
leA :: AInt -> AInt
leA (AInt xs) = case xs of
  []         -> AInt []
  In a _ : _ -> AInt [In NegInf a]

-- toPred :: Expr -> AInt -> Pred
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
--   mkRel r i = PRel r lhs (ECon (I i NoPV))
--   holeRels  = map (mkRel Ne) (holes xs)

instance Pretty AInt where
  pretty (AInt [])  = emptySet
  pretty (AInt [x]) = pretty x
  pretty (AInt xs)  = PP.encloseSep lbracket rbracket mid 
                    $ map (\(In a b) -> pretty a <> comma <> pretty b) xs



addI :: AInt -> Integer -> AInt
addI (AInt xs) i = case xs of
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
    | otherwise = brackets $ pretty a <> comma <> pretty b

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
  pretty NegInf = "-" <> symInf
  pretty PosInf = "+" <> symInf
  pretty (Fin a) = pretty a
