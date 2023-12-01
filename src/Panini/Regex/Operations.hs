{-|
This module implements 'intersection', 'complement', and 'equivalence' of
regular expressions and it does so entirely algebraically, without intermediate
translation into automata.

References:

  * Acay, Josh. "A Regular Expression Library for Haskell." Unpublished
    manuscript, dated May 22, 2018. LaTeX files and Haskell source code.
    https://github.com/cacay/regexp

  * Antimirov, Valentin. 1996. "Partial derivatives of regular expressions and
    finite automaton constructions." Theoretical Computer Science 155 (1996):
    291-319. https://doi.org/10.1016/0304-3975(95)00182-4

  * Arden, Dean N. 1961. "Delayed Logic and Finite State Machines." Proceedings
    of the 2nd Annual Symposium on Switching Circuit Theory and Logical Design
    (SWCT 1961), 133-151. https://doi.org/10.1109/FOCS.1961.13

  * Brzozowski, Janusz A. 1964. "Derivatives of Regular Expressions." Journal of
    the ACM 11, no. 4 (October 1964): 481-494.
    https://doi.org/10.1145/321239.321249

  * Keil, Matthias and Peter Thiemann. 2014. "Symbolic Solving of Extended
    Regular Expression Inequalities." https://arxiv.org/abs/1410.3227
    
  * Liang Tianyi, Nestan Tsiskaridze, Andrew Reynolds, Cesare Tinelli, and Clark
    Barrett. 2015. "A decision procedure for regular membership and length
    constraints over unbounded strings." Frontiers of Combining Systems (FroCoS
    2015), LNAI 9322, 135–150. https://doi.org/10.1007/978-3-319-24246-0_9
-}
module Panini.Regex.Operations where

import Algebra.Lattice
import Control.Exception
import Control.Monad.Trans.State.Strict
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Semigroup hiding (All)
import Data.Set (Set)
import Data.Set qualified as Set
import Panini.Regex.CharSet (CharSet)
import Panini.Regex.CharSet qualified as CS
import Panini.Regex.Type
import Prelude

-------------------------------------------------------------------------------

-- | Compute the intersection of two regexes.
--
-- The implementation works purely algebraically, without going through a DFA.
-- It is based on the equation-solving approach by Acay (unpublished manuscript,
-- 2018), using Arden's lemma (1961) and Gaussian elimination to solve a system
-- of regex equations. The approach is similar to the one by Liang et al. (2015,
-- Fig. 8), where the interplay of the π and ρ functions essentially does the
-- same thing. Like Acay, we also make use of the local mintermization approach
-- by Keil and Thiemann (2014) to effectively compute precise derivatives over
-- large alphabets (see the 'next' function below).
intersection :: Regex -> Regex -> Regex
intersection = curry $ solve $ \(r1,r2) ->
  let c0 = if nullable r1 && nullable r2 then One else Zero
      cx = [ ((derivative c r1, derivative c r2), Lit p) 
           | p <- Set.toList $ next r1 ⋈ next r2
           , Just c <- [CS.choose p]
           ]
  in (c0, Map.fromList cx)

-- | Compute the complement of a regex.
--
-- See the 'intersection' operation for notes on the implementation.
complement :: Regex -> Regex
complement = solve $ \r ->
  let c0 = if nullable r then Zero else One
      c1 = Lit (neg $ joins $ next r) <> All
      cx = [ (derivative c r, Lit p)
           | p <- Set.toList $ next r
           , Just c <- [CS.choose p]
           ]
  in (Plus [c0,c1], Map.fromList cx)

-------------------------------------------------------------------------------

-- TODO: implement equivalence checking more directly/efficiently

-- | Semantic equivalence of two regular expressions, i.e., language equality.
equivalence :: Regex -> Regex -> Bool
equivalence r1 r2 =
  Plus [intersection r1 (complement r2), intersection (complement r1) r2] == Zero

-------------------------------------------------------------------------------

-- | A system of regex equations.
type System x = Map x (RHS x)

-- | The right-hand side of an equation @Xᵢ = c₀ + c₁⋅X₁ + c₂⋅X₂ + … + cₙXₙ@,
-- with @c₀@ being a known constant term. Each unknown term @c⋅X@, consisting of
-- a coefficient @c@ and a variable @X@, is represented as an entry in a map
-- from @X@ to @c@.
type RHS x = (Regex, Map x Regex)

-- | @solve f X₀@ dynamically constructs and solves a system of linear regex
-- equations, given an initial unknown variable @X₀@ and a function @f@ that
-- computes the right-hand side of any unknown variable.
solve :: forall x. Ord x => (x -> RHS x) -> x -> Regex
solve f x0 = evalState (go x0) mempty
 where
  go :: x -> State (System x) Regex
  go x = do
    s <- gets $ Map.lookup x
    case s of
      Just (c0,xs) ->
        assert (null xs)
        return c0

      Nothing -> do
        (c0,ys) <- elim x <$> resolve (f x)
        modify' $ Map.insert x (c0,ys)
        cs <- mapM (\(y,c) -> (c <>) <$> go y) (Map.toList ys)
        let c0' = Plus (c0:cs)
        modify' $ Map.insert x (c0', mempty)
        return c0'

  resolve :: RHS x -> State (System x) (RHS x)
  resolve (c0,xs) = do
    rs <- mapM resolveTerm (Map.toList xs)
    return $ foldr combine (c0, mempty) rs

  resolveTerm :: (x, Regex) -> State (System x) (RHS x)
  resolveTerm (x,c) = do
    s <- gets $ Map.lookup x
    case s of
      Just (c0,xs) -> scale c <$> resolve (c0,xs)
      _            -> return (Zero, Map.singleton x c)

  elim :: x -> RHS x -> RHS x
  elim x (c0,xs) = 
    case Map.lookup x xs of
      Nothing -> (c0,xs)
      Just cx -> assert (not $ nullable cx)
                 scale (Star cx) (c0, Map.delete x xs)

  scale :: Regex -> RHS x -> RHS x
  scale r (c0,xs) = (r <> c0, Map.map (r <>) xs)

  combine :: RHS x -> RHS x -> RHS x
  combine (c01,xs1) (c02,xs2) = (Plus [c01,c02], Map.unionWith (\a b -> Plus [a,b]) xs1 xs2)

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
derivative c = \case
  One               -> Zero
  Lit d 
    | CS.member c d -> One
    | otherwise     -> Zero
  Plus []           -> Zero
  Plus [r]          -> derivative c r
  Plus rs           -> Plus $ map (derivative c) rs
  Times []          -> Zero
  Times [r]         -> derivative c r
  Times (r:rs) 
    | nullable r    -> Plus [Times (derivative c r : rs), derivative c (Times rs)]
    | otherwise     -> (derivative c r) <> Times rs
  Star r            -> (derivative c r) <> Star r
  Opt r             -> derivative c r

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
  One            -> Set.singleton bot
  Lit a          -> Set.singleton a
  Plus []        -> Set.singleton bot
  Plus [r]       -> next r
  Plus (r:rs)    -> next r ⋈ next (Plus rs)
  Times []       -> Set.singleton bot
  Times [r]      -> next r
  Times (r:rs)
    | nullable r -> next r ⋈ next (Times rs)
    | otherwise  -> next r
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
