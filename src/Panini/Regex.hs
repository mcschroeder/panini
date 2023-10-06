{-# LANGUAGE OverloadedLists #-}

-- based on Liang et. al. 2015. A Decision Procedure for Regular Membership and
-- Length Constraints over Unbounded Strings.

-- charset trick / local mintermization due to Keil and Thiemann. 2014. Symbolic
-- Solving of Regular Expression Inequalities.

 -- Brzozowski (1964).
-- Valentin Antimirov , 1996. Partial derivatives of regular expressions and finite automaton constructions . Theoretical Computer Science 155 (1996) 291-319
module Panini.Regex where

import Algebra.Lattice hiding (join)
import Data.Generics.Uniplate.Direct
import Prelude
import Data.Set (Set)
import Data.Set qualified as Set
import Panini.Pretty
import Panini.Abstract.AChar (AChar)
import Panini.Abstract.AChar qualified as AChar

-------------------------------------------------------------------------------

data Regex
  = One
  | Lit AChar
  | Plus Regex Regex
  | Times Regex Regex
  | Star Regex
  deriving stock (Eq, Ord)

pattern Zero :: Regex
pattern Zero <- Lit (isBot -> True) where
  Zero = Lit bot

instance JoinSemilattice Regex where
  (∨) = Plus

instance BoundedJoinSemilattice Regex where
  bot = Zero

instance MeetSemilattice Regex where
  (∧) = intersection

instance BoundedMeetSemilattice Regex where
  top = Star (Lit top)

instance Uniplate Regex where
  uniplate = \case
    One         -> plate One
    Lit c       -> plate Lit |- c
    Plus r1 r2  -> plate Plus |* r1 |* r2
    Times r1 r2 -> plate Times |* r1 |* r2
    Star r      -> plate Star |* r

instance Show Regex where
  showsPrec d = \case
    One         -> showString "ε"
    Lit c       -> showString $ showPretty c
    Plus  r1 r2 -> showParen (d > 6) $ showsPrec 6 r1 . showString "|" . showsPrec 7 r2
    Times r1 r2 -> showParen (d > 7) $ showsPrec 7 r1 . showsPrec 8 r2
    Star r      -> showParen (d > 10) $ showsPrec 11 r . showString "*"

-------------------------------------------------------------------------------

-- | Rewrite a regex into a succinct normal form (Liang et al. 2015, Fig. 3).
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

-- | A regex is said to be /nullable/ if it accepts the empty string.
nullable :: Regex -> Bool
nullable = \case
  One         -> True
  Lit _       -> False
  Plus  r1 r2 -> nullable r1 || nullable r2
  Times r1 r2 -> nullable r1 && nullable r2
  Star _      -> True

-------------------------------------------------------------------------------

-- | Compute the intersection of two regexes.
--
-- The implementation works purely algebraically, without going through a DFA.
-- It is based on the π function by Liang et al. (2015, Fig. 8) but does not
-- introduce intermediate variables into the regex. It also uses the local
-- mintermization approach by Keil and Thiemann (2014) to effectively compute
-- precise derivates over large alphabets (see the 'next' function below).
intersection :: Regex -> Regex -> Regex
intersection = π []
 where
  π _ Zero _                       = Zero
  π _ _    Zero                    = Zero
  π _ One  r    | not (nullable r) = Zero
  π _ r    One  | not (nullable r) = Zero
  π _ One  r    | nullable r       = One
  π _ r    One  | nullable r       = One
  π _ r1   r2   | r1 == r2         = r1
  π m r1   r2   | otherwise        = foldl ρ r0 rs
   where
    m' = (r1,r2):m

    ρ r (p, (d1,d2))
      | (d1,d2) `elem` m' = Star (Lit p) `Times` r
      | otherwise         = r `Plus` (Lit p `Times` π m' d1 d2)
    
    r0 | nullable r1, nullable r2 = One 
       | otherwise                = Zero
    
    rs = [ (p, (derivative c r1, derivative c r2))
         | p <- Set.toList $ next r1 ⋈ next r2
         , Just c <- [AChar.choose p]
         ]

-------------------------------------------------------------------------------

-- | The derivative c⁻¹r of a regex r with respect to a character c is a new
-- regex that accepts all words that would be accepted by r if they were
-- prefixed by c, i.e., ℒ(c⁻¹r) = { w | cw ∈ ℒ(r) }.
--
-- Regular expression derivatives were first introduced by Brzozowski (1964).
-- The notation c⁻¹ is due to Antimirov (1996), who also introduced the notion
-- of /partial/ derivatives. Note that in the literature, the partial derivative
-- operator ∂ is sometimes used to denote (non-partial) Brzozowski derivatives.
-- Both Keil and Thiemann (2014) and Liang et al. (2015) make this mistake, with
-- the latter even erroneously claiming to define the partial derivative
-- function while giving the classic Brzozowski definition (Fig. 6).
derivative :: Char -> Regex -> Regex
derivative c = normalize . \case
  One        -> Zero
  Plus r1 r2 -> derivative c r1 `Plus` derivative c r2
  Star r     -> derivative c r `Times` Star r
  Lit d 
    | c `AChar.member` d -> One
    | otherwise          -> Zero
  Times r1 r2 
    | nullable r1 -> (derivative c r1 `Times` r2) `Plus` derivative c r2
    | otherwise   -> (derivative c r1 `Times` r2)

-------------------------------------------------------------------------------

-- | The  /next literals/ of a regex are a set {A₁,A₂,...,Aₙ} of mutually
-- disjoint character sets Aᵢ such that all symbols in each character set yield
-- the same derivative. This allows us to avoid enumerating the entire alphabet
-- during 'intersection': "[T]o determine a finite set of representatives for
-- all derivatives of a regular expression r it is sufficient to select one
-- symbol a from each equivalence class A ∈ next(r)∖{∅} and calculate ∂ₐ(r)."
-- (Keil and Thiemann 2014, section 5.2)
next :: Regex -> Set AChar
next = \case
  One             -> [bot]
  Lit a           -> [a]
  Star r          -> next r
  Plus r1 r2      -> next r1 ⋈ next r2
  Times r1 r2 
    | nullable r1 -> next r1 ⋈ next r2
    | otherwise   -> next r1

-- | Given two sets of mutually disjoint literals, ⨝ (join) builds a new set of
-- mutually disjoint literals that covers the union of the two sets (Keil and
-- Thiemann 2014, Definition 7).
(⋈) :: Set AChar -> Set AChar -> Set AChar
l1 ⋈ l2 = Set.fromList $ concat $
  [ [ a1 ∧ a2
    , a1 ∧ (neg $ joins l2)
    , a2 ∧ (neg $ joins l1)
    ]
  | a1 <- Set.toList l1, a2 <- Set.toList l2
  ]
