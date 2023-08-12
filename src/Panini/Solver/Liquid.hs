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
import Panini.Monad
import Panini.Pretty
import Panini.SMT.Z3
import Panini.Solver.Assignment
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude

-- | Solve non-nested constrained Horn clauses (CHCs) given a set of candidates.
-- Returns 'Nothing' if no solution could be found.
solve :: [FlatCon] -> [Pred] -> Pan (Maybe Assignment)
solve cs qs = do
  logMessage $ "Find Horn-headed constraints" <+> sym_csk
  let (csk,csp) = partition horny cs
  logData csk

  logMessage $ "Construct initial solution" <+> sigma <> subscript 0
  -- TODO: we assume free vars in qs to match the k param names (z1,...,zn)
  -- this is clearly not good
  let ks = kvars csk
  let qs' = if null qs then PTrue else PAnd qs
  let s0 = Map.fromList $ map (\k -> (k, qs')) $ Set.toList ks
  logData s0

  logMessage $ "Iteratively weaken" <+> sigma <+> 
               "until all" <+> sym_csk <+> "are satisfied"
  s <- fixpoint csk s0
  logData s

  logMessage $ "Apply" <+> sigma <+> "to concrete constraints" <+> sym_csp
  let csp2 = map (apply s) csp
  logData csp2

  logMessage $ "Validate" <+> sigma <> parens sym_csp
  smtValid csp2 >>= \case
    True -> return $ Just s
    False -> return Nothing
 
 where
  sym_csk = "csₖ" `orASCII` "cs_k"
  sym_csp = "csₚ" `orASCII` "cs_p"

-- | Whether or not a flat constraint has a κ application in its head.
horny :: FlatCon -> Bool
horny (FAll _ _ (PAppK _ _)) = True
horny _                      = False

-- | Iteratively weaken a candidate solution until an assignment satisfying all
-- given constraints is found.
fixpoint :: [FlatCon] -> Assignment -> Pan Assignment
fixpoint cs s = do  
  r <- take 1 <$> filterM ((not <$>) . smtValid . pure . apply s) cs
  case r of
    [c] -> fixpoint cs =<< weaken s c
    _   -> return s

-- | Weaken an assignment to satisfy a given constraint.
weaken :: Assignment -> FlatCon -> Pan Assignment
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
