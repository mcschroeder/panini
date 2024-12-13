-------------------------------------------------------------------------------
-- | An implementation of the FUSION algorithm for local refinement typing.
-- 
-- Reference:
--
--   * Benjamin Cosman and Ranjit Jhala. 2017. Local Refinement Typing.
--     ICFP. https://doi.org/10.1145/3110270
-------------------------------------------------------------------------------
module Panini.Solver.Fusion (solve) where

import Algebra.Lattice
import Control.Monad
import Data.Bifunctor
import Data.Graph qualified as Graph
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set ((\\))
import Panini.Solver.Assignment
import Panini.Solver.Constraints
import Panini.Solver.Simplifier
import Panini.Monad
import Panini.Syntax
import Prelude
import Panini.Pretty
import Panini.Environment (SolverError(..))

-- | Use refinement FUSION to eliminate all acyclic κ variables that are not in
-- the given exclusion set, returning the (partially) solved constraint.
solve :: Set KVar -> Con -> Pan SolverError Con
solve ksx c0 = do
  ksc <- cutVars c0                  § "Compute cut variables"
  ks  <- (kvars c0 \\ ksc \\ ksx)    § "Identify non-excluded non-cut variables"
  c1  <- elim (Set.toAscList ks) c0  
  return c1
  
-- | Eliminates a set of acyclic κ-variables iteratively via 'elim1'.
elim :: [KVar] -> Con -> Pan SolverError Con
elim ks c0 = foldM elimOne c0 ks
 where
  elimOne c1 k = do
    c2 <- elim1 k c1     § "Eliminate" <+> pretty k
    c3 <- simplify c2    § "Simplify constraint"
    return c3

-- | Eliminates κ from a constraint c by invoking 'elim'' on the strongest
-- scoped solution for κ in c.
elim1 :: KVar -> Con -> Con
elim1 k c = elim' sk c
  where
    sk = Map.singleton k (sol1 k c')
    c' = skipHypos $ scope k c

-- | Given a constraint of the form ∀x₁:b₁. p₁ ⇒ … ⇒ ∀xₙ:bₙ. pₙ ⇒ c',
-- @skipHypos@ will omit the hypotheses p₁,…,pₙ and return just c'. 
skipHypos :: Con -> Con
skipHypos (CAll _ _ _ c) = skipHypos c
skipHypos c              = c

-- | @sol1 κ c@ returns a predicate that is guaranteed to satisfy all clauses in
-- c where κ appears as the head.
sol1 :: KVar -> Con -> Pred
sol1 k (CAnd c1 c2)   = sol1 k c1 ∨ sol1 k c2
sol1 k (CAll x b p c) = mkExists x b (p ∧ sol1 k c)
sol1 k (CHead (PAppK k2 ys))
  | k == k2           = PAnd $ map (\(x,y) -> PRel $ EVar x :=: y)
                             $ zip (kparams k) ys
sol1 _ _              = PFalse

mkExists :: Name -> Base -> Pred -> Pred
mkExists x b p
  | x `elem` freeVars p = PExists x b p
  | otherwise           = p

-- | @scope κ c@ returns a sub-constraint of c of the form ∀x₁:b₁. p₁ ⇒ … ⇒
-- ∀xₙ:bₙ. pₙ ⇒ c' such that κ does not occur in p₁,…,pₙ, and all occurrences of
-- κ in c occur in c'. Any solution of κ can omit the hypotheses p₁,…,pₙ because
-- they are already present at all /uses/ of κ.
scope :: KVar -> Con -> Con
scope k (CAnd c1 c2)
  | k    `elem` kvars c1, k `notElem` kvars c2 = scope k c1
  | k `notElem` kvars c1, k    `elem` kvars c2 = scope k c2
scope k (CAll x b p c')
  | k `notElem` kvars p                        = CAll x b p $ scope k c'
scope _ c                                      = c

-- | Eliminates all κ-variables from a constraint by replacing each
-- body-occurrence of a κ by σ(κ) and each head-occurrence of a κ by /true./
elim' :: Assignment -> Con -> Con
elim' s (CAnd c1 c2)   = elim' s c1 ∧ elim' s c2
elim' s (CAll x b p c) = CAll x b (apply s p) (elim' s c)
elim' s (CHead (PAppK k _)) 
  | k `Map.member` s   = CTrue
elim' _ c              = c

-------------------------------------------------------------------------------

-- | The dependencies @deps c@ between the κ variables of a constraint @c@ is
-- the set of pairs (κ,κ') where κ appears in a body (hypothesis) for a clause
-- where κ' is in the head (goal).
deps :: Con -> Set (KVar, KVar)
deps = Set.unions . map depsF . flat
  where
    depsF (FAll _ p q) = Set.cartesianProduct (kvars p) (kvars q)

-- | A set Κ̂ of κ-variables /cuts/ a constraint c if the dependencies between
-- the κ variables in c /without Κ̂/ are acyclic. In other words, @cutVars c@
-- returns those κ variables of @c@ that make the satisfaction problem for @c@
-- undecidable.
cutVars :: Con -> Set KVar
cutVars = go mempty . deps
  where
    go ks ds = case findCyclicK (scc ds) of
      Just k  -> go (Set.insert k ks) (depsK k ds)
      Nothing -> ks

    -- removes the given κ variable from the dependency set
    depsK :: KVar -> Set (KVar, KVar) -> Set (KVar, KVar)
    depsK k = Set.filter (\(a,b) -> a /= k && b /= k)

    -- the strongly connected components (SCCs) of the dependency set
    scc :: Set (KVar, KVar) -> [Graph.SCC KVar]
    scc = Graph.stronglyConnComp . adjList

    -- an adjacency list representation of the dependency set (cf. Data.Graph)
    adjList :: Set (KVar, KVar) -> [(KVar, Int, [Int])]
    adjList = map (\(k@(KVar i _ _), is) -> (k, i, is))
            . Map.toList
            . Map.fromAscListWith (++) 
            . map (second (\(KVar i _ _) -> [i])) 
            . Set.toAscList

    -- the first cyclic κ variable in a list of SCCs
    findCyclicK :: [Graph.SCC KVar] -> Maybe KVar
    findCyclicK xs = case xs of
      Graph.CyclicSCC (k:_) : _  -> Just k
      []                         -> Nothing
      _                     : vs -> findCyclicK vs
