-------------------------------------------------------------------------------
-- | An implementation of the FUSION algorithm for local refinement typing.
-- 
-- Reference:
--
--   * Benjamin Cosman and Ranjit Jhala. 2017. Local Refinement Typing.
--     ICFP. https://doi.org/10.1145/3110270
-------------------------------------------------------------------------------
module Panini.Solver.Fusion (sat) where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Panini.Solver.Assignment
import Panini.Solver.Liquid
import Panini.Solver.Simplify
import Panini.Syntax
import Prelude

sat :: Con -> [Pred] -> IO Bool
sat c q = do
  let ks = kvars c
  let ks' = Set.toList ks  -- TODO: cut set
  let c1 = simplify c
  -- putStrLn "--- simplified constraint:"
  -- outputPretty c1
  let c' = simplify $ elim ks' c1
  -- putStrLn "--- after fusion:"
  -- outputPretty c'
  -- putStrLn "---"
  r <- solve c' q
  case r of
    Just _s -> do
      -- forM_ (Map.toList s) $ \(k,(xs,p)) -> do
      --   outputPretty $ (PRel Eq (PHorn k (map V xs)) p)
      return True
    Nothing -> return False

-- | Eliminates a set of acyclic κ-variables iteratively via 'elim1'.
elim :: [KVar] -> Con -> Con
elim []     c = c
elim (k:ks) c = elim ks (elim1 k c)

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
sol1 k (CAnd c1 c2)   = (sol1 k c1) `pOr` (sol1 k c2)
sol1 k (CAll x b p c) = PExists x b (p `pAnd` sol1 k c)
sol1 k (CHead (PAppK k2 ys))
  | k == k2           = PAnd $ map (\(x,y) -> PVar x `pEq` PVar y) 
                             $ zip (kparams k) ys
sol1 _ _              = PFalse NoPV

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
elim' s (CAnd c1 c2)   = elim' s c1 `cAnd` elim' s c2
elim' s (CAll x b p c) = CAll x b (apply s p) (elim' s c)
elim' s (CHead (PAppK k _)) 
  | k `Map.member` s   = CTrue NoPV
elim' _ c              = c
