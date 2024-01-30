module Panini.Solver
  ( solve
  , module Panini.Solver.Assignment
  ) where

import Control.Monad.Extra
import Data.Foldable
import Data.Function
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Panini.Monad
import Panini.Pretty
import Panini.Solver.Abstract (allPreCons, preConKVar)
import Panini.Solver.Abstract qualified as Abstract
import Panini.Solver.Assignment
import Panini.Solver.Constraints
import Panini.Solver.Fusion qualified as Fusion
import Panini.Solver.Liquid qualified as Liquid
import Panini.Solver.Qualifiers
import Panini.Solver.Simplifier
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

solve :: Set KVar -> Con -> Pan (Maybe Assignment)
solve kst c0 = do
  c1  <- simplifyCon c0                   § "Simplify constraint"
  ksp <- allPreConKVars c1                § "Identify precondition variables"
  c2  <- Fusion.solve (kst <> ksp) c1  
  c3  <- simplifyCon c2                   § "Simplify constraint"  
  sp  <- Abstract.solve c3
  c4  <- apply sp c3                      § "Apply precondition solution"
  c5  <- simplifyCon c4                   § "Simplify constraint"  
  kts <- qualifierTypes c5                § "Identify qualifier types"
  qs  <- extractQualifiers' c0 kts        § "Extract candidate qualifiers"
  cs5 <- flat c5                          § "Flatten constraint"

  logMessage $ "Find approximate solutions for residual" <+> kappa <+> "variables"  
  Liquid.solve cs5 qs >>= \case
    Nothing -> return Nothing
    Just sl -> do
      logMessage "Found a valid solution!"
      let s0 = Map.fromList [(k, PTrue) | k <- toList $ kvars c0]
      let s  = sp <> sl <> s0
      logData s
      return $ Just s
 
 where
  -- TODO
  qualifierTypes c = Set.fromList [ts | KVar _ ts <- toList $ kvars c]
  extractQualifiers' c kts = Map.fromList [ (ts, extractQualifiers c ts) | ts <- toList $ kts]
  allPreConKVars = Set.fromList . map preConKVar . toList . allPreCons
