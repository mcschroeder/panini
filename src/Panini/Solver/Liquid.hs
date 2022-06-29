-------------------------------------------------------------------------------
-- | An implementation of Horn constraint solving via predicate abstraction.
--
-- References:
--
--   * Ranjit Jhala and Niki Vazou. 2020. Refinement Types: A Tutorial.
--     arXiv. https://doi.org/10.48550/arXiv.2010.07763
--
--   * Patrick M. Rondon, Ming Kawaguci, Ranjit Jhala. 2008. Liquid Types.
--     PLDI. https://doi.org/10.1145/1375581.1375602
-------------------------------------------------------------------------------
module Panini.Solver.Liquid (solve) where

import Control.Monad
import Data.List (partition)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Panini.Solver.Assignment
import Panini.Solver.Z3
import Panini.Syntax
import Prelude

-- | Solve a Horn constraint given a set of candidates.
solve :: Con -> [Pred] -> IO (Maybe Assignment)
solve c qs = do
  let cs = flat c
  -- putStrLn "---"
  -- mapM_ (putStrLn . showPretty) cs
  -- putStrLn "---"
  let (csk,csp) = partition horny cs
  
  -- TODO: we assume free vars in qs to match the k param names (z1,...,zn)
  -- this is clearly not good
  let ks = kvars csk
  let s0 = Map.fromList $ map (\k -> (k, PAnd qs)) $ Set.toList ks
  
  s <- fixpoint csk s0
  r <- smtValid (map (apply s) csp)
  if r then return (Just s) else return Nothing

-- | Flatten a Horn constraint into a set of flat constraints each of which is
-- of the form ∀x1:b1. p1 ⇒ ∀x2:b2. p2 ⇒ ... ⇒ pn where pn is either a single
-- Horn application κ(ȳ) or a concrete predicate free of Horn variables.
flat :: Con -> [Con]
flat = split
  where
    split (CHead p)      = [CHead p]
    split (CAnd c1 c2)   = split c1 ++ split c2
    split (CAll x b p c) = [CAll x b p c' | c' <- split c]

-- | Whether or not a flat constraint has a Horn application in its head.
horny :: Con -> Bool
horny (CAll _ _ _ (CHead (PAppK _ _))) = True
horny _                                   = False

-- | Iteratively weaken a candidate solution until an assignment satisfying all
-- given (flat) constraints is found.
fixpoint :: [Con] -> Assignment -> IO Assignment
fixpoint cs s = do  
  r <- take 1 <$> filterM ((not <$>) . smtValid . pure . apply s) cs
  case r of
    [c] -> do
      s' <- weaken s c
      fixpoint cs s'
    _ -> return s

-- | Weaken a Horn assignment to satisfy a given (flat) constraint.
weaken :: Assignment -> Con -> IO Assignment
weaken s c =
  case flatHead c of
    PAppK k xs -> case Map.lookup k s of
      Nothing -> error $ "expected Horn assignment for " ++ show k
      Just q0 -> do
        let c' = mapFlatBody (apply s) c
        let keep q = smtValid [mapFlatHead (const (substN xs (kparams k) q)) c']
        qs' <- PAnd <$> filterM keep (explode q0)
        return $ Map.insert k qs' s

    _ -> error "expected Horn variable at head of flat constraint"

explode :: Pred -> [Pred]
explode (PAnd ps) = ps
explode p = [p]

flatHead :: Con -> Pred
flatHead (CAll _ _ _ c) = flatHead c
flatHead (CHead p) = p
flatHead (CAnd _ _) = error "expected flat constraint"

mapFlatBody :: (Pred -> Pred) -> Con -> Con
mapFlatBody f (CAll x b p c) = CAll x b (f p) (mapFlatBody f c)
mapFlatBody _ (CHead p) = CHead p
mapFlatBody _ (CAnd _ _) = error "expected flat constraint"

mapFlatHead :: (Pred -> Pred) -> Con -> Con
mapFlatHead f (CAll x b p c) = CAll x b p (mapFlatHead f c)
mapFlatHead f (CHead p) = CHead (f p)
mapFlatHead _ (CAnd _ _) = error "expected flat constraint"
