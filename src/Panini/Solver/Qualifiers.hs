module Panini.Solver.Qualifiers (extractQualifiers, extractQualifiers2) where

import Algebra.Lattice
import Data.Bifunctor
import Data.Containers.ListUtils
import Data.Foldable
import Data.Generics.Uniplate.Operations
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Panini.Provenance
import Panini.Solver.Assignment
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude

-- TODO: we assume kparams are always named z0,...,zn
-- TODO: normalize the candidate predicates before eliminating duplicates

------------------------------------------------------------------------------

-- | Given a constraint and a type signature b₁,b₂,…,bₙ, we extract candidate
-- qualifiers for all subsequences of the type signature (i.e., we allow some
-- parameters to be unassigned) based on the following heuristics:
--
--   1) We take all relations appearing in the constraint that contain free
--      variables exactly matching (subsequences of) the given types.
--
--   2) For all singleton types, we form simple constant relations for all
--      constant values appearing in the constraint.
--
-- The returned predicates are ready to be substituted for κ variables.
extractQualifiers :: Con -> [Base] -> [Pred]
extractQualifiers c bs = nubOrd $ map normPred $
  [ PRel r' | bs' <- List.subsequences (zip bs zs)
            , not (null bs')
            , r <- toList $ relationsOver (map fst bs') c
            , let m = Map.fromListWith (++) $ map (second pure) bs'
            , r' <- renameVars m r ]
  ++ [ PRel $ Rel op (EVar z) a | (b,z) <- zip bs zs, (op,a) <- constants b ]
 where
  zs = [Name ("z" <> Text.pack (show i)) NoPV | i <- [0..] :: [Int]]
  constants = \case
    TUnit   -> [ (Eq, EUnit    NoPV) ]
    TBool   -> [ (Eq, EBool b  NoPV) | b <- [True,False] ]
    TInt    -> [ (op, EInt  i  NoPV) | EInt  i  _ <- universeBi c, op <- [Eq,Ne,Gt,Ge,Lt,Le] ]
    TChar   -> [ (Eq, EChar ch NoPV) | EChar ch _ <- universeBi c ]
    TString -> [ (Eq, EStr  s  NoPV) | EStr  s  _ <- universeBi c ]

-- TODO: normalize qualifiers more aggressively to avoid redundancies
normPred :: Pred -> Pred
normPred = \case
  PRel (EVar x :>: EInt i pv) -> PRel (EVar x :≥: EInt (i + 1) pv)
  PRel (EVar x :<: EInt i pv) -> PRel (EVar x :≤: EInt (i - 1) pv)
  PRel (EVar x :≠: EBool b pv) -> PRel (EVar x :=: EBool (not b) pv)
  p -> p

-- | Rename the variables in a relation according to a given (multi-)mapping
-- based on type, exhausting all possibilities. Note: the given map is expected
-- to cover all variable types appearing in the relation!
renameVars :: Map Base [Name] -> Rel -> [Rel]
renameVars m0 r0 = [ substN (map EVar zs) xs r0 
                   | xzs <- renamings m0 (toList $ freeVarsWithTypes r0)
                   , let (xs,zs) = unzip xzs
                   ]
 where
  renamings _        []  = [[]]
  renamings m ((x,b):xs) = concatMap go $ fromJust $ Map.lookup b m
   where 
      go y = map ((x,y):) $ renamings (Map.adjust (List.\\ [y]) b m) xs    

-- | Returns the free variables in a relation if they all have known types;
-- otherwise, returns an empty set.
freeVarsWithTypes :: Rel -> Set (Name,Base)
freeVarsWithTypes r = maybe mempty Set.fromList 
                    $ sequenceA 
                    $ [ fmap (x,) (typeOfVarInRel x r) 
                      | x <- Set.toList $ freeVars r ]

-- | Extract all relations from a constraint that involve exactly the given
-- types as free variables (in any order, with repetitions).
relationsOver :: [Base] -> Con -> Set Rel
relationsOver bs0 c = Set.fromList
  [ r | PRel r <- universeBi c
      , let rbs = List.sort $ map snd $ toList $ freeVarsWithTypes r
      , rbs == bs
  ] 
 where bs = List.sort bs0

-- TODO: systematize and unify qualifier extraction heuristics

-- TODO: maybe this should be done on the list of FlatCons?
extractQualifiers2 :: Con -> Assignment
extractQualifiers2 c = Map.fromList
  [ (k, meets [ PRel $ EVar "z0" :≤: EStrLen (EVar x)
              , PRel $ EVar "z0" :<: EStrLen (EVar x) ])
  | k@(KVar _ [TInt] _) <- Set.toList $ kvars c
  , (x, TString) <- collectVarsInScopeOfKVar k c  
  ]

collectVarsInScopeOfKVar :: KVar -> Con -> [(Name,Base)]
collectVarsInScopeOfKVar k = Set.toList . go mempty
 where
  go vs = \case
    CAnd c1 c2 -> go vs c1 `Set.intersection` go vs c2
    CAll x b p c 
      | containsK p -> Set.insert (x,b) vs
      | otherwise -> go (Set.insert (x,b) vs) c
    CHead p | containsK p -> vs
            | otherwise -> mempty

  containsK p = or [True | PAppK x _ <- universe p, x == k]
