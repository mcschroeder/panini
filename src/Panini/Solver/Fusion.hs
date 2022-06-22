-------------------------------------------------------------------------------
-- | An implementation of the FUSION algorithm for local refinement typing.
-- 
-- Reference:
--
--   * Benjamin Cosman and Ranjit Jhala. 2017. Local Refinement Typing.
--     ICFP. https://doi.org/10.1145/3110270
-------------------------------------------------------------------------------
module Panini.Solver.Fusion (sat) where

import Data.List (nub)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String
import Panini.Provenance
import Panini.Solver.Assignment
import Panini.Solver.Liquid
import Panini.Solver.Simplify
import Panini.Syntax
import Prelude

sat :: Con -> [Pred] -> IO Bool
sat c q = do
  let ks = getKs c
  let ks' = ks  -- TODO: cut set
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

type K = (Name,[Name])

getKs :: Con -> [K]
getKs = Set.toList . Set.fromList . go
  where
    go (CHead p) = go2 p
    go (CAnd c1 c2) = go c1 ++ go c2
    go (CAll _ _ p c) = go2 p ++ go c
    go2 (PBin _ p1 p2) = go2 p1 ++ go2 p2
    go2 (PRel _ p1 p2) = go2 p1 ++ go2 p2
    go2 (PAnd ps) = concatMap go2 ps
    go2 (PDisj p1 p2) = go2 p1 ++ go2 p2
    go2 (PImpl p1 p2) = go2 p1 ++ go2 p2
    go2 (PIff p1 p2) = go2 p1 ++ go2 p2
    go2 (PNot p) = go2 p
    go2 (PExists _ _ p) = go2 p
    go2 (PHorn k xs) = [(k, [fromString ("z" ++ show i) | i <- [1..length xs]])]
    go2 _ = []

elim :: [K] -> Con -> Con
elim []     c = c
elim (k:ks) c = elim ks (elim1 k c)

elim1 :: K -> Con -> Con
elim1 k c = {- trace (prettyAss sk) $ -} elim' sk c
  where
    sk = Map.singleton (fst k) (snd k, sol1 k c')
    c' = flatHead2 $ scope k c

flatHead2 :: Con -> Con
flatHead2 (CAll _ _ _ c) = flatHead2 c
flatHead2 c = c

varnames :: [Value] -> [Name]
varnames = map go
  where
    go (V x) = x
    go _ = error "expected all xs in k(xs) to be variables"


sol1 :: K -> Con -> Pred
sol1 k (CAnd c1 c2)   = (sol1 k c1) `pOr` (sol1 k c2)
sol1 k (CAll x b p c) = PExists x b (p `pAnd` sol1 k c)
sol1 k (CHead (PHorn k2 ys))
  | fst k == k2       = PAnd $ map (\(x,y) -> pVar x `pEq` pVar y) 
                             $ zip (snd k) (varnames ys)
sol1 _ _              = PFalse NoPV

scope :: K -> Con -> Con
scope k (CAnd c1 c2)
  | k `elem` kvars c1, k `notElem` kvars c2 = scope k c1
  | k `notElem` kvars c1, k `elem` kvars c2 = scope k c2
scope k (CAll x b p c')
  | k `notElem` kvars p                     = CAll x b p (scope k c')
scope _ c                                   = c

elim' :: Assignment -> Con -> Con
elim' s (CAnd c1 c2)   = elim' s c1 `cAnd` elim' s c2
elim' s (CAll x b p c) = CAll x b (apply s p) (elim' s c)
elim' s (CHead (PHorn k _)) 
  | k `Map.member` s   = CTrue NoPV
elim' _ c              = c


class HasKVars a where
  kvars :: a -> [K]

instance HasKVars Pred where
  kvars = nub . go
    where
      go (PHorn k xs) = [(k, varnames xs)]  -- xs assumed to be just var names
      go (PBin _ p1 p2) = kvars p1 ++ kvars p2
      go (PRel _ p1 p2) = kvars p1 ++ kvars p2
      go (PAnd ps) = concatMap kvars ps
      go (PDisj p1 p2) = kvars p1 ++ kvars p2
      go (PImpl p1 p2) = kvars p1 ++ kvars p2
      go (PIff p1 p2) = kvars p1 ++ kvars p2
      go (PNot p) = kvars p
      go _ = []

instance HasKVars Con where
  kvars = nub . go
    where
      go (CHead p) = kvars p
      go (CAnd c1 c2) = kvars c1 ++ kvars c2
      go (CAll _ _ p c) = kvars p ++ kvars c
