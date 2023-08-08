module Panini.Solver
  ( solve
  , module Panini.Solver.Assignment
  ) where

import Control.Monad
import Data.Function
import Data.HashSet qualified as HashSet
import Data.Map qualified as Map
import Data.Set qualified as Set
import Panini.Monad
import Panini.Pretty.Printer
import Panini.Solver.Assignment
import Panini.Solver.Constraints
import Panini.Solver.Fusion qualified as Fusion
import Panini.Solver.Grammar (gconKVar)
import Panini.Solver.Grammar qualified as Grammar
import Panini.Solver.Liquid qualified as Liquid
import Panini.Solver.Simplify
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

-- TODO: be strict in each of these steps
-- TODO: explore simplification, whether it is or isn't necessary/profitable

solve :: Con -> Pan (Maybe Assignment)
solve c0 = do

  let c1 = c0 -- TODO: investigate simplification here

  logMessage "Find grammar constraints"
  let gcs1 = Grammar.grammarConstraints c1
  logData gcs1

  logMessage "Extract grammar variables"
  let ks_gram = Set.fromList $ map gconKVar $ HashSet.toList gcs1
  logData ks_gram

  logMessage $ "Eliminate acyclic" <+> symKappa <+> "variables"
  c2 <- Fusion.solve ks_gram c1
  
  -- NOTE: We assume σ(κ) = true for all κ variables that were eliminated during
  -- Fusion so that later we can (trivially) fill all type signature holes
  -- without existentials leaking into the types.
  --
  -- CAVEAT: This might not be correct (and at the very least leads to a loss of
  -- precision), but it seems to work for now for our purposes.
  --
  -- TODO: Revisit this issue.
  let s_fusion = Map.fromList $ zip (Set.toList $ kvars c0) (repeat PTrue)

  logMessage "Simplify"
  let !c3 = simplifyCon c2 -- TODO: disable this and make it work regardless
  logData c3

  logMessage "Find remaining grammar constraints"
  let gcs3 = Grammar.grammarConstraints c3
  logData gcs3

  logMessage $ "Infer grammars"
  s_grammar <- Grammar.solveAll gcs3

  logMessage "Apply grammar solution"
  let !c4 = apply s_grammar c3
  logData c4

  let !c5 = c4 -- TODO: investigate simplification here

  logMessage "Flatten constraint"
  let cs5 = flat c5
  logData cs5

  logMessage $ "Extract candidate qualifiers" <+> "ℚ" `orASCII` "Q"
  let qs = [PTrue] -- TODO: extract Q from... type signatures?
  logData qs

  logMessage $ 
    "Find approximate solutions for residual" <+> symKappa <+> "variables"
  Liquid.solve cs5 [] >>= \case
    Nothing -> return Nothing
    Just s_liquid -> do
      logMessage "Found a valid solution!"
      let s_final = Map.unions [s_grammar, s_liquid, s_fusion]
      logData s_final
      return $ Just s_final
