module Panini.Solver.Qualifiers (extractQualifiers) where

import Data.Generics.Uniplate.Operations
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text qualified as Text
import Panini.Provenance
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude

-- TODO: expand set of extracted qualifiers
-- TODO: we assume kparams are always named z0,...,zn
-- TODO: we assume the first type is for z0 aka the value variable v
extractQualifiers :: Con -> [Base] -> [Pred]
extractQualifiers con = 
  List.nub . concatMap (quals con) . List.nub . List.subsequences . zip [0..]

-- TODO: improve brittleness of renaming etc.

quals :: Con -> [(Int,Base)] -> [Pred]
quals con = \case  
  [(_,TUnit)] -> [PTrue]
  [(i,TBool)] -> [ PRel $ Rel op (z i) e2 | PRel (Rel op (EVar _) e2@(EBool _ _)) <- universeBi con ]
  [(i,TInt)] -> [ PRel $ Rel op (z i) e2 | PRel (Rel op (EVar _) e2@(EInt _ _)) <- universeBi con ]
  [(i,TInt),(j,TInt)] ->  [ PRel $ substN [z i, z j] xs r
                  | PRel r <- universeBi con 
                  , let fvs = freeVars r, Set.size fvs == 2
                  , xs <- List.permutations $ Set.toList fvs
                  ]
  _ -> []
 where
  z i = EVar $ Name ("z" <> Text.pack (show i)) NoPV
