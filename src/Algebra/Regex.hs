{-# LANGUAGE OverloadedLists #-}

-- based on Liang et. al. 2015. A Decision Procedure for Regular Membership and
-- Length Constraints over Unbounded Strings.

-- charset trick / local mintermization due to Keil and Thiemann. 2014. Symbolic
-- Solving of Regular Expression Inequalities.

module Algebra.Regex where

import Algebra.Lattice hiding (join)

import Data.Generics.Uniplate.Direct
import Prelude
import Data.Set (Set)
import Data.Set qualified as Set
import Data.List qualified as List
import Data.Map qualified as Map
import Panini.Pretty

import Panini.Abstract.AChar (AChar)
import Panini.Abstract.AChar qualified as AChar

import Debug.Trace
import Data.Either

-- TODO: integrate intersection/meet into language?
-- TODO: use charsets

data Regex
  = One
  | Lit AChar
  | Plus Regex Regex
  | Times Regex Regex
  | Star Regex
  | Var (Regex,Regex)
  deriving stock (Eq, Ord)

pattern Zero :: Regex
pattern Zero <- Lit (isBot -> True) where
  Zero = Lit bot

instance Uniplate Regex where
  uniplate = \case
    One -> plate One
    Lit c -> plate Lit |- c
    Plus r1 r2 -> plate Plus |* r1 |* r2
    Times r1 r2 -> plate Times |* r1 |* r2
    Star r -> plate Star |* r
    Var y -> plate Var |- y

instance Show Regex where
  showsPrec d = \case
    One         -> showString "ε"
    Lit c       -> showString $ showPretty c
    Plus  r1 r2 -> showParen (d > 6) $ showsPrec 6 r1 . showString "|" . showsPrec 7 r2
    Times r1 r2 -> showParen (d > 7) $ showsPrec 7 r1 . showsPrec 8 r2
    Star r      -> showParen (d > 10) $ showsPrec 11 r . showString "*"
    Var y -> showString $ "X_" ++ show y

normalize :: Regex -> Regex
normalize = rewrite $ \case
  Plus r1 r2 | r1 == r2 -> Just r1
  Plus Zero r -> Just r
  Plus r Zero -> Just r
  Times (Times r1 r2) r3 -> Just $ Times r1 (Times r2 r3)
  Times r One -> Just r
  Times One r -> Just r
  Times r1 (Plus r2 r3) -> Just $ Plus (Times r1 r2) (Times r1 r3)
  Times (Plus r1 r2) r3 -> Just $ Plus (Times r1 r3) (Times r2 r3)
  Times Zero _  -> Just Zero
  Times _ Zero -> Just Zero
  Star (Star r) -> Just $ Star r
  Star (Plus r One) -> Just $ Star r
  Star (Plus One r) -> Just $ Star r
  Star Zero -> Just One
  Star One -> Just One
  _ -> Nothing



nullable :: Regex -> Bool
nullable = \case
  One         -> True
  Lit _       -> False
  Plus  r1 r2 -> nullable r1 || nullable r2
  Times r1 r2 -> nullable r1 && nullable r2
  Star _      -> True
  Var _ -> False

π :: Regex -> Regex -> Regex
π = π' []
 where
  π' _ Zero _                       = Zero
  π' _ _    Zero                    = Zero
  π' _ One  r    | not (nullable r) = Zero
  π' _ r    One  | not (nullable r) = Zero
  π' _ One  r    | nullable r       = One
  π' _ r    One  | nullable r       = One
  π' _ r1   r2   | r1 == r2         = r1
  π' m r1   r2   | Just y <- lookup (r1,r2) m = y
  π' m r1   r2   | otherwise        = Times (Star r1') r2'
   where
    (r1', r2') = ρ (r1,r2) r'
    r' | nullable r1, nullable r2 = foldl Plus One  rs
       | otherwise                = foldl Plus Zero rs
    rs = [ Lit p `Times` π' m' (derivative c r1) (derivative c r2)
         | p <- Set.toList $ join (next r1) (next r2)
         , Just c <- [AChar.choose p]
         ]
    m' = ((r1,r2), Var (r1,r2)):m

  ρ _ Zero = (Zero, Zero)
  ρ y (Var y') | y == y' = (One, Zero)
  ρ y r | y `notElem` vars r = (One, r)
  ρ y r | Times r1 r2 <- r, (r21,r22) <- ρ y r2 = (Times r1 r21, r22)
  ρ y r | Plus r1 r2 <- r, (r11,r12) <- ρ y r1, (r21,r22) <- ρ y r2 = (Plus r11 r21, Plus r12 r22)
    

vars :: Regex -> [(Regex,Regex)]
vars = \case
  One         -> []
  Lit _       -> []
  Plus  r1 r2 -> vars r1 ++ vars r2
  Times r1 r2 -> vars r1 ++ vars r2
  Star r      -> vars r
  Var y -> [y]
  

intersection :: Regex -> Regex -> Regex
intersection = curry $ \case
  (_,Lit c) | isBot c -> Lit bot
  (Lit c,_) | isBot c -> Lit bot
  (One,r)
    | nullable r -> One
    | otherwise  -> Lit bot
  (r,One)
    | nullable r -> One
    | otherwise  -> Lit bot
  (r,r')
    | r == r' -> r
    | otherwise -> foldl Plus r0 rs
        where
          r0 | nullable r, nullable r' = One
             | otherwise = Lit bot
          rs = [ traceShow (r', r) $ Lit p `Times` (normalize (intersection (derivative c r) (derivative c r')))
               | p <- Set.toList $ join (next r') (next r)
               , Just c <- [AChar.choose p]
               ]


next :: Regex -> Set AChar
next = \case
  One -> [bot]
  Lit a -> [a]
  Plus r1 r2 -> next r1 `join` next r2
  Times r1 r2 | nullable r1 -> next r1 `join` next r2
              | otherwise   -> next r1
  Star r -> next r

join :: Set AChar -> Set AChar -> Set AChar
join l1 l2 = Set.fromList $ concat $
  [ [ a1 ∧ a2
    , a1 ∧ (neg $ joins l2)
    , a2 ∧ (neg $ joins l1)
    ]
  | a1 <- Set.toList l1, a2 <- Set.toList l2
  ]

derivative :: Char -> Regex -> Regex
derivative c = normalize . \case
  Zero -> Zero
  One             -> Zero
  Plus r1 r2      -> Plus (derivative c r1) (derivative c r2)
  Star r          -> Times (derivative c r) (Star r)
  Lit d
    | c `AChar.member` d -> One
    | otherwise          -> Zero
  Times r1 r2 
    | nullable r1 -> Plus (Times (derivative c r1) r2) (derivative c r2)
    | otherwise   -> Times (derivative c r1) r2
    

