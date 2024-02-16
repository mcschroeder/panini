module Panini.Solver
  ( solve
  , Result(..)
  , module Panini.Solver.Assignment
  ) where

import Data.Foldable
import Data.Function
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Panini.Monad
import Panini.SMT.Z3 qualified as Z3
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
import Algebra.Lattice

-------------------------------------------------------------------------------

-- | The result of trying to solve a verification condition with κ variables.
data Result 
  = Valid Assignment  -- ^ the VC is valid under this assignment
  | Invalid           -- ^ we could not find any valid assignment
  | Unverified Assignment String  
      -- ^ we found a possible assignment but could not finally verify it; 
      --   the string gives the reason why (e.g., "timeout")

solve :: Set KVar -> Con -> Pan Result
solve kst c0 = do
  logMessage "Phase 1: FUSION — Eliminate local acyclic variables"
  c1  <- simplifyCon c0                  § "Simplify constraint"
  c2  <- Fusion.solve kst c1
  c3  <- simplifyCon c2                  § "Simplify constraint"

  logMessage "Phase 2: LIQUID — Solve residual non-precondition variables"
  ksp <- allPreConKVars c3               § "Identify precondition variables"
  c4  <- apply (allTrue ksp) c3          § "Temporarily eliminate preconditions"
  c5  <- simplifyCon c4                  § "Simplify constraint"
  qs  <- qualifiers c0 (kvars c5)        § "Extract candidate qualifiers"
  cs5 <- flat c5                         § "Flatten constraint"
  csk <- filter horny cs5                § "Gather Horn-headed constraints"
  s0  <- solution0 qs (kvars csk)        § "Construct initial solution"
  sl  <- Liquid.fixpoint csk s0
  c6  <- c3                              § "Restore preconditions"
  c7  <- apply sl c6                     § "Apply Liquid solution"
  c8  <- simplifyCon c7                  § "Simplify constraint"

  logMessage "Phase 3: ABSTRACT — Find weakest preconditions"
  sa  <- Abstract.solve c8
  c9  <- apply sa c8                     § "Apply abstract solution"
  c10 <- simplifyCon c9                  § "Simplify constraint"
  s   <- sa <> sl <> allTrue (kvars c0)  § "Construct final solution"
  vc  <- apply s c10                     § "Apply final solution"

  logMessage "Phase 4: VERIFY — Validate final verification condition"
  vcs <- flat vc                         § "Flatten constraint"
  res <- Z3.smtCheck vcs
  case res of
    Z3.Sat       -> return (Valid s)
    Z3.Unknown u -> return (Unverified s u)
    Z3.Unsat     -> return Invalid

 where
  allTrue         = Map.fromSet (const PTrue)
  allPreConKVars  = Set.fromList . map preConKVar . toList . allPreCons
  qualifiers c ks = Map.fromSet (extractQualifiers c) (Set.map ktypes ks)
  solution0 qs ks = Map.fromSet (maybe PTrue meets . flip Map.lookup qs . ktypes) ks
