module Panini.Solver
  ( solve
  , module Panini.Solver.Assignment
  ) where

import Control.Monad
import Data.Function
import Data.Generics.Uniplate.Operations
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Panini.Monad
import Panini.Panic
import Panini.Pretty
import Panini.Provenance
import Panini.Solver.Assignment
import Panini.Solver.Constraints
import Panini.Solver.Fusion qualified as Fusion
import Panini.Solver.Grammar (gconKVar)
import Panini.Solver.Grammar qualified as Grammar
import Panini.Solver.Liquid qualified as Liquid
import Panini.Solver.Simplifier
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

-- TODO: expand set of extracted qualifiers
-- TODO: we assume kparams are always named z0,...,zn
extractQualifiers :: Con -> [Base] -> [Pred]
extractQualifiers con = List.nub . \case
  [TUnit] -> [PTrue]
  [TBool] -> [ PRel $ EVar "z0" :=: EBool True NoPV
             , PRel $ EVar "z0" :=: EBool False NoPV 
             ]
  [TInt] -> [ PRel (Rel op (EVar "z0") e2) 
            | PRel (Rel op (EVar _) e2@(EInt _ _)) <- universeBi con
            ]
  [TInt, TInt] -> 
    [ PRel $ substN [EVar "z0", EVar "z1"] xs r
    | PRel r <- universeBi con 
    , let fvs = freeVars r, Set.size fvs == 2
    , xs <- List.permutations $ Set.toList fvs
    ]
  ts -> panic $ "extractQualifiers" <+> pretty ts <+> "not implemented"


solve :: Set KVar -> Con -> Pan (Maybe Assignment)
solve ks_ex c1 = do  
  let gcs1 = Grammar.grammarConstraints c1  
  let ks_gram = Set.fromList $ map gconKVar $ HashSet.toList gcs1
  logMessage $ "Grammar variables:" <+> pretty ks_gram

  logMessage $ "Eliminate local acyclic" <+> kappa <+> "variables"
  c2 <- Fusion.solve (ks_ex <> ks_gram) c1
  
  c3 <- simplify c2

  logMessage "Find remaining grammar constraints"
  let gcs3 = Grammar.grammarConstraints c3
  logData gcs3

  logMessage $ "Infer grammars"
  s_grammar <- Grammar.solveAll gcs3

  logMessage "Apply grammar solution"
  let !c4 = apply s_grammar c2
  logData c4

  let !c5 = c4 -- TODO: investigate simplification here

  logMessage "Flatten constraint"
  let cs5 = flat c5
  logData cs5

  logMessage $ "Extract candidate qualifiers" <+> "ℚ" `orASCII` "Q"
  let kts = List.nub [ts | KVar _ ts <- Set.toList $ kvars c5]
  let qs = Map.fromList [(ts, extractQualifiers c5 ts) | ts <- kts]
  logData qs

  logMessage $ 
    "Find approximate solutions for residual" <+> kappa <+> "variables"
  Liquid.solve cs5 qs >>= \case
    Nothing -> return Nothing
    Just s_liquid -> do
      logMessage "Found a valid solution!"
      let s_final = Map.unions [s_grammar, s_liquid]
      logData s_final
      return $ Just s_final
