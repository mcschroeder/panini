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
import Panini.Error
import Panini.Monad
import Panini.Pretty.Printer
import Panini.SMT.Z3
import Panini.Solver.Assignment
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude
import Control.Monad.Extra (unlessM)

-- | Solve non-nested constrained Horn clauses (CHCs) given a set of candidates.
solve :: [FlatCon] -> [Pred] -> Pan Assignment
solve cs qs = do
  logMessage $ "Find Horn-headed constraints" <+> sym_csk
  let (csk,csp) = partition horny cs
  logData csk

  logMessage $ "Construct initial solution" <+> "σ₀" `orASCII` "\\sigma_0"
  -- TODO: we assume free vars in qs to match the k param names (z1,...,zn)
  -- this is clearly not good
  let ks = kvars csk
  let qs' = if null qs then PTrue else PAnd qs
  let s0 = Map.fromList $ map (\k -> (k, qs')) $ Set.toList ks
  logData s0

  logMessage $ "Iteratively weaken" <+> symSigma <+> 
               "until all" <+> sym_csk <+> "are satisfied"
  s <- fixpoint csk s0
  logData s

  logMessage $ "Apply" <+> symSigma <+> "to concrete constraints" <+> sym_csp
  let csp2 = map (apply s) csp
  logData csp2

  logMessage $ "Validate" <+> symSigma <> parens sym_csp
  unlessM (smtValid csp2) $
    throwError $ SolverError "Unsatisfiable"  -- TODO: proper error

  return s
 
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
