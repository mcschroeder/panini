{-# LANGUAGE OverloadedStrings #-}

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
import Panini.Solver.SMTLIB
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


-- | A flat constraint of the form ∀(x₁:b₁,x₂:b₂,…). p ⇒ q where q is either a
-- single κ-variable application κ(y̅) or a concrete predicate free of
-- κ-variables.
data FlatCon = FAll [(Name,Base)] Pred Pred

instance HasKVars FlatCon where
  kvars (FAll _ p q) = kvars p <> kvars q
  apply s (FAll xs p q) = FAll xs (apply s p) (apply s q)

instance SMTLib2 FlatCon where
  encode (FAll xs p q) = sexpr ["forall", sorts, impl]
    where
      sorts = sexpr $ map sort xs
      sort (x,b) = sexpr [encode x, encode b]
      impl = sexpr ["=>", encode p, encode q]

-- | Flatten a constraint.
flat :: Con -> [FlatCon]
flat c₀ = [simpl [] [PTrue NoPV] c' | c' <- split c₀]
  where
    split (CAll x b p c) = [CAll x b p c' | c' <- split c]
    split (CHead p)      = [CHead p]
    split (CAnd c₁ c₂)   = split c₁ ++ split c₂

    simpl xs ps (CAll x b p c) = simpl ((x,b):xs) (p:ps) c
    simpl xs ps (CHead q)      = FAll (reverse xs) (PAnd $ reverse ps) q
    simpl _  _  (CAnd _ _)     = error "impossible"

-- | Whether or not a flat constraint has a κ application in its head.
horny :: FlatCon -> Bool
horny (FAll _ _ (PAppK _ _)) = True
horny _                      = False

-- | Iteratively weaken a candidate solution until an assignment satisfying all
-- given constraints is found.
fixpoint :: [FlatCon] -> Assignment -> IO Assignment
fixpoint cs s = do  
  r <- take 1 <$> filterM ((not <$>) . smtValid . pure . apply s) cs
  case r of
    [c] -> fixpoint cs =<< weaken s c
    _   -> return s

-- | Weaken an assignment to satisfy a given constraint.
weaken :: Assignment -> FlatCon -> IO Assignment
weaken s (FAll xs p (PAppK k ys)) =
  case Map.lookup k s of
    Nothing -> error $ "missing Horn assignment for " ++ show k
    Just q0 -> do
      let p' = apply s p
      let keep q = smtValid [FAll xs p' (substN ys (kparams k) q)]
      qs' <- PAnd <$> filterM keep (explode q0)
      return $ Map.insert k qs' s

weaken _ _ = error "impossible"

explode :: Pred -> [Pred]
explode (PAnd ps) = ps
explode p         = [p]
