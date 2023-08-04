module Panini.Solver
  ( solve
  , module Panini.Solver.Assignment
  ) where

import Control.Monad
import Data.Function
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Panini.Monad
import Panini.Pretty.Printer
import Panini.Solver.Assignment
import Panini.Solver.Constraints
import Panini.Solver.Fusion qualified as Fusion
import Panini.Solver.Grammar (GCon(..), gconKVar)
import Panini.Solver.Grammar qualified as Grammar
import Panini.Solver.Liquid qualified as Liquid
import Panini.Solver.Simplify
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

-- TODO: be strict in each of these steps
-- TODO: explore simplification, whether it is or isn't necessary/profitable

solve :: Con -> Pan Assignment
solve c0 = do

  let c1 = c0 -- TODO: investigate simplification here

  logMessage "Find grammar constraints"
  let gcs1 = Grammar.grammarConstraints c1
  logData gcs1

  let ks_gram = Set.fromList 
              $ map gconKVar 
              $ HashSet.toList gcs1
  logMessage $ "Grammar variables =" <+> pretty ks_gram

  c2 <- Fusion.solve ks_gram c1

  logMessage "Simplify"
  let !c3 = simplifyCon c2 -- TODO: disable this and make it work regardless
  logData c3

  logMessage "Find grammar constraints"
  let gcs3 = Grammar.grammarConstraints c3
  logData gcs3

  let solveOne s (GCon x k c) = do
        logMessage $ "Solve grammar variable" <+> pretty k
        -- update grammar consequent with previous grammar solutions
        let gc' = GCon x k $ apply s c
        let g = Grammar.solve gc'
        logData g
        return $ g `Map.union` s

  s_grammar <- foldM solveOne mempty 
             $ List.sortBy (compare `on` gconKVar)
             $ HashSet.toList gcs3


  logMessage "Apply grammar solution"
  let !c4 = apply s_grammar c3
  logData c4

  let !c5 = c4 -- TODO: investigate simplification here

  logMessage "Compute approximate solutions for residuals"
  !s_liquid <- Liquid.solve c5 []
  
  logMessage "Construct final solution"
  -- NOTE: We assume σ(κ) = true for all κ variables that were eliminated during
  -- Fusion so that we can (trivially) fill all type holes without existentials
  -- leaking into the types.   
  --
  -- CAVEAT: This might not be correct (and at the very least leads to a loss of
  -- precision), but it seems to work for now for our purposes.
  --
  -- TODO: Revisit this issue.
  let s_trues = Map.fromList $ zip (Set.toList $ kvars c0) (repeat PTrue)  
  let s_final = Map.unions [s_grammar, s_liquid] `Map.union` s_trues
  logData s_final
  
  return s_final
