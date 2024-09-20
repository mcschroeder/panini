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
solve kst c00 = do
  c0  <- uniquifyBoundVariables c00      § "Ensure uniqueness of bound variables"

  logMessage "Phase 1: FUSION — Eliminate local acyclic variables"
  c1  <- simplifyCon c0                  § "Simplify constraint"
  c2  <- Fusion.solve kst c1
  c3  <- simplifyCon c2                  § "Simplify constraint"

  logMessage "Phase 2: LIQUID — Solve residual non-grammar variables"
  ksp <- allGrammarVars c3               § "Identify grammar variables"
  c4  <- apply (allTrue ksp) c3          § "Hide grammar variables"
  c5  <- simplifyCon c4                  § "Simplify constraint"
  qs  <- qualifiers c0 (kvars c5)        § "Extract candidate qualifiers"
  cs5 <- flat c5                         § "Flatten constraint"
  csk <- filter horny cs5                § "Gather Horn-headed constraints"
  s0  <- solution0 qs (kvars csk)        § "Construct initial solution"
  sl  <- Liquid.fixpoint csk s0
  c6  <- c3                              § "Restore grammar variables"
  c7  <- apply sl c6                     § "Apply Liquid solution"
  c8  <- simplifyCon c7                  § "Simplify constraint"

  logMessage "Phase 3: ABSTRACT — Infer grammars using abstract interpretation"
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
  allGrammarVars  = Set.filter (([TString] ==) . ktypes) . allPreConKVars
  qualifiers c ks = Map.fromSet (extractQualifiers c) (Set.map ktypes ks)
  solution0 qs ks = Map.fromSet (maybe PTrue meets . flip Map.lookup qs . ktypes) ks


-- | Ensure that ∀ and ∃ don't shadow other bound variables.
uniquifyBoundVariables :: Con -> Con
uniquifyBoundVariables = go mempty
 where
  go xs = \case  
    CHead p       -> CHead (goP xs p)
    CAnd c1 c2    -> CAnd (go xs c1) (go xs c2)
    CAll x b p c
      | Set.member x xs
      , let x'  = freshName x xs
      , let xs' = Set.insert x' xs
      , let p'  = subst (EVar x') x p
      , let c'  = subst (EVar x') x c
                  -> CAll x' b (goP xs' p') (go xs' c')
      | let xs' = Set.insert x xs 
                  -> CAll x  b (goP xs' p ) (go xs' c )  
  goP xs = \case
    PTrue         -> PTrue
    PFalse        -> PFalse
    PAnd ps       -> PAnd $ map (goP xs) ps
    POr ps        -> POr $ map (goP xs) ps
    PImpl p q     -> PImpl (goP xs p) (goP xs q)
    PIff p q      -> PIff (goP xs p) (goP xs q)
    PNot p        -> PNot (goP xs p)
    PRel r        -> PRel r
    PAppK k ys    -> PAppK k ys
    PExists x b p
      | Set.member x xs
      , let x'  = freshName x xs
      , let xs' = Set.insert x' xs
      , let p'  = subst (EVar x') x p
                  -> PExists x' b (goP xs' p')
      | otherwise -> PExists x b (goP xs p)
