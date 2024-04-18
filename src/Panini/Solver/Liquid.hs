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
module Panini.Solver.Liquid (solve, fixpoint) where

import Algebra.Lattice (meets)
import Control.Monad
import Data.List (partition)
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Set qualified as Set
import Panini.Monad
import Panini.Panic
import Panini.Pretty
import Panini.SMT.Z3
import Panini.Solver.Assignment
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude

-- | Solve non-nested constrained Horn clauses (CHCs) given a set of candidates.
-- Returns 'Nothing' if no solution could be found.
solve :: [FlatCon] -> Map [Base] [Pred] -> Pan (Maybe Assignment)
solve cs qs = do
  logMessage $ "Find Horn-headed constraints" <+> sym_csk
  let (csk,csp) = partition horny cs
  logData $ sym_csk <+> symEq <+> pretty csk

  logMessage $ "Construct initial solution" <+> sigma <> subscript 0
  -- TODO: we still assume free vars in qs to match the k param names (z0,...,zn)
  let s0 = Map.fromList
         [ (k, q) | k@(KVar _ ts) <- Set.toList (kvars cs)
         , let q = maybe PTrue meets (Map.lookup ts qs)
         ]
  logData $ sigma <> subscript 0 <+> symEq <+> pretty s0

  logMessage $ "Iteratively weaken" <+> sigma <+> 
               "until all" <+> sym_csk <+> "are satisfied"
  s <- fixpoint csk s0
  logMessage "Found fixpoint"
  logData $ sigma <+> symEq <+> pretty s

  logMessage $ "Apply" <+> sigma <+> "to concrete constraints" <+> sym_csp
  let csp2 = map (apply s) csp
  logData $ sigma <> parens sym_csp <+> symEq <+> pretty csp2

  logMessage $ "Validate" <+> sigma <> parens sym_csp
  smtCheck csp2 >>= \case
    Sat -> return $ Just s
    _   -> return Nothing
 
 where
  sym_csk = "csₖ" `orASCII` "cs_k"
  sym_csp = "csₚ" `orASCII` "cs_p"

-- | Iteratively weaken a candidate solution until an assignment satisfying all
-- given constraints is found.
fixpoint :: [FlatCon] -> Assignment -> Pan Assignment
fixpoint cs s = do
  logData $ sigma <+> symEq <+> pretty s
  r <- filterM ((not . isSat <$>) . smtCheck . pure . apply s) cs
  case r of
    []  -> return s
    c:_ -> fixpoint cs =<< weaken s c

-- | Weaken an assignment to satisfy a given constraint.
weaken :: Assignment -> FlatCon -> Pan Assignment
weaken s (FAll xs p (PAppK k ys)) =
  case Map.lookup k s of
    Nothing -> panic $ "missing Horn assignment for" <+> pretty k
    Just q0 -> do
      let p' = apply s p
      let keep q = isSat <$> smtCheck [FAll xs p' (substN ys (kparams k) q)]
      qs' <- meets <$> filterM keep (explode q0)
      return $ Map.insert k qs' s

weaken _ _ = impossible

explode :: Pred -> [Pred]
explode (PAnd ps) = ps
explode p         = [p]
