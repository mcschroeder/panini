
-- based on Liang et. al. 2015. A Decision Procedure for Regular Membership and
-- Length Constraints over Unbounded Strings.

module Algebra.Regex where

import Data.Generics.Uniplate.Direct
import Data.Generics.Uniplate.Operations
import Prelude

alphabet :: [Char]
alphabet = "abcdefg"

-- TODO: integrate intersection/meet into language?
-- TODO: use charsets
-- TODO: how to enumerate infinite alphabet?

data Regex
  = Zero
  | One
  | Lit Char
  | Plus Regex Regex
  | Times Regex Regex
  | Star Regex
  deriving stock (Eq, Ord)

instance Uniplate Regex where
  uniplate = \case
    Zero -> plate Zero
    One -> plate One
    Lit c -> plate Lit |- c
    Plus r1 r2 -> plate Plus |* r1 |* r2
    Times r1 r2 -> plate Times |* r1 |* r2
    Star r -> plate Star |* r

instance Show Regex where
  showsPrec d = \case
    Zero        -> showString "∅"
    One         -> showString "ε"
    Lit c       -> showChar c
    Plus  r1 r2 -> showParen (d > 6) $ showsPrec 6 r1 . showString "|" . showsPrec 7 r2
    Times r1 r2 -> showParen (d > 7) $ showsPrec 7 r1 . showsPrec 8 r2
    Star r      -> showParen (d > 10) $ showsPrec 11 r . showString "*"

normalize :: Regex -> Regex
normalize = rewrite $ \case
  Plus r1 r2 | r1 == r2 -> Just r1
  Plus Zero r  -> Just r
  Plus r Zero -> Just r
  Times (Times r1 r2) r3 -> Just $ Times r1 (Times r2 r3)
  Times r One -> Just r
  Times One r -> Just r
  Times r1 (Plus r2 r3) -> Just $ Plus (Times r1 r2) (Times r1 r3)
  Times (Plus r1 r2) r3 -> Just $ Plus (Times r1 r3) (Times r2 r3)
  Times Zero _ -> Just Zero
  Times _ Zero -> Just Zero
  Star (Star r) -> Just $ Star r
  Star (Plus r One) -> Just $ Star r
  Star Zero -> Just One
  Star One -> Just One
  _ -> Nothing

nullable :: Regex -> Bool
nullable = \case
  Zero        -> False
  One         -> True
  Lit _       -> False
  Plus  r1 r2 -> nullable r1 || nullable r2
  Times r1 r2 -> nullable r1 && nullable r2
  Star _      -> True

intersection :: Regex -> Regex -> Regex
intersection = curry $ \case
  (_,Zero) -> Zero
  (Zero,_) -> Zero
  (One,r)
    | nullable r -> One
    | otherwise  -> Zero
  (r,One)
    | nullable r -> One
    | otherwise  -> Zero
  (r,r')
    | r == r' -> r
  (r,r') 
    | nullable r, nullable r' -> (Star r1) `Times` r1'
      where
        (r1,r1') = rho (One `Plus` derivs)
        derivs   = foldl1 Plus $ map deriv alphabet
        deriv c  = (Lit c) `Times` (intersection (derivative c r) (derivative c r'))
  (r,r') 
    | otherwise -> Star r1 `Times` r1'
      where
        (r1, r1') = rho derivs
        derivs    = foldl1 Plus $ map deriv alphabet
        deriv c   = (Lit c) `Times` (intersection (derivative c r) (derivative c r'))

rho :: Regex -> (Regex, Regex)
rho = \case
  Zero -> (Zero, Zero)
  r    -> (One, r)

derivative :: Char -> Regex -> Regex
derivative c = normalize . \case
  Zero            -> Zero
  One             -> Zero
  Lit d
    | c == d      -> One
    | otherwise   -> Zero
  Star r          -> (derivative c r) `Times` Star r
  Plus r1 r2      -> (derivative c r1) `Plus` (derivative c r2)
  Times r1 r2 
    | nullable r1 -> ((derivative c r1) `Times` r2) `Plus` (derivative c r2)
    | otherwise   -> (derivative c r1) `Times` r2
    

