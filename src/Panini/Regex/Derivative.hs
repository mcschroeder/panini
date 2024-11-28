{-|

References:

  * Antimirov, Valentin. 1996. "Partial derivatives of regular expressions and
    finite automaton constructions." Theoretical Computer Science 155 (1996):
    291-319. https://doi.org/10.1016/0304-3975(95)00182-4

  * Brzozowski, Janusz A. 1964. "Derivatives of Regular Expressions." Journal of
    the ACM 11, no. 4 (October 1964): 481-494.
    https://doi.org/10.1145/321239.321249

  * Keil, Matthias and Peter Thiemann. 2014. "Symbolic Solving of Extended
    Regular Expression Inequalities." https://arxiv.org/abs/1410.3227

-}
module Panini.Regex.Derivative where

import Algebra.Lattice
import Data.Set (Set)
import Data.Set qualified as Set
import Panini.Regex.CharSet (CharSet)
import Panini.Regex.CharSet qualified as CS
import Panini.Regex.Type
import Prelude

-------------------------------------------------------------------------------

-- | The derivative c⁻¹r of a regex r with respect to a character c is a new
-- regex that accepts all words that would be accepted by r if they were
-- prefixed by c, i.e., ℒ(c⁻¹r) = { w | cw ∈ ℒ(r) }.
--
-- Regular expression derivatives were first introduced by Brzozowski (1964).
-- The notation c⁻¹ is due to Antimirov (1996), who also introduced the notion
-- of /partial/ derivatives. Note that in the literature, the partial derivative
-- operator ∂ is sometimes used to denote (non-partial) Brzozowski derivatives,
-- as in Keil and Thiemann (2014).
derivative :: Char -> Regex -> Regex
derivative c = go
 where
  go = \case
    One               -> Zero
    Lit d 
      | CS.member c d -> One
      | otherwise     -> Zero
    Plus1 r rs        -> go r `plus` go rs
    Times1 r rs 
      | nullable r    -> (go r `times` rs) `plus` go rs
      | otherwise     -> go r `times` rs 
    Star r            -> go r `times` Star r
    Opt r             -> go r

-------------------------------------------------------------------------------

-- | The  /next literals/ of a regex are a set {A₁,A₂,...,Aₙ} of mutually
-- disjoint character sets Aᵢ such that all symbols in each character set yield
-- the same derivative. This allows us to avoid enumerating the entire alphabet
-- during 'intersection': "[T]o determine a finite set of representatives for
-- all derivatives of a regular expression r it is sufficient to select one
-- symbol a from each equivalence class A ∈ next(r)∖{∅} and calculate ∂ₐ(r)."
-- (Keil and Thiemann 2014, section 5.2; note that ∂ here denotes the Brzozowski
-- derivative)
next :: Regex -> Set CharSet
next = \case
  One            -> Set.singleton CS.empty
  Lit a          -> Set.singleton a
  Plus1 x y      -> next x ⋈ next y
  Times1 x y
    | nullable x -> next x ⋈ next y
    | otherwise  -> next x
  Star r         -> next r
  Opt r          -> next One ⋈ next r

-- | Given two sets of mutually disjoint literals, ⨝ (join) builds a new set of
-- mutually disjoint literals that covers the union of the two sets (Keil and
-- Thiemann 2014, Definition 7).
(⋈) :: Set CharSet -> Set CharSet -> Set CharSet
l1 ⋈ l2 = Set.fromList $ concat $
  [ [ a1 ∧ a2
    , a1 ∧ (neg $ joins l2)
    , a2 ∧ (neg $ joins l1)
    ]
  | a1 <- Set.toList l1, a2 <- Set.toList l2
  ]

-- | A left-biased version of ⋈ that only covers the symbols of its left operand
-- (Keil and Thiemann 2014, Definition 16).
(⋉) :: Set CharSet -> Set CharSet -> Set CharSet
l1 ⋉ l2 = Set.fromList $ concat $
  [ [ a1 ∧ a2
    , a1 ∧ (neg $ joins l2)
    ]
  | a1 <- Set.toList l1, a2 <- Set.toList l2
  ]
