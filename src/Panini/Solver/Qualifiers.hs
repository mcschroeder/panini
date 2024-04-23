module Panini.Solver.Qualifiers (extractQualifiers) where

import Data.Bifunctor
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
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude

-- TODO: we assume kparams are always named z0,...,zn

------------------------------------------------------------------------------

-- | Candidate qualifiers for a type signature b₁,b₂,…,bₙ are extracted from a
-- constraint based on all relations appearing in the constraint that contain
-- free variables matching any subsequence of the given type signature.
extractQualifiers :: Con -> [Base] -> [Pred]
extractQualifiers c bs = List.nub
  [ PRel r' | bs' <- List.subsequences (zip bs zs)
            , not (null bs')
            , r <- toList $ relationsOver (map fst bs') c
            , let m = Map.fromListWith (++) $ map (second pure) bs'
            , r' <- renameVars m r 
  ]
 where
  zs = [Name ("z" <> Text.pack (show i)) NoPV | i <- [0..] :: [Int]]

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
