module Panini.Solver.Qualifiers (extractQualifiers) where

import Data.Generics.Uniplate.Operations
import Data.List qualified as List
import Data.Text qualified as Text
import Panini.Provenance
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude

-- TODO: expand set of extracted qualifiers
-- TODO: we assume kparams are always named z0,...,zn
-- TODO: we assume the first type is for z0 aka the value variable v
extractQualifiers :: Con -> [Base] -> [Pred]
extractQualifiers con 
  = List.nub 
  . concatMap (quals con)
  . List.nub 
  . filter ((0 ==) . fst . head)  -- only consider Qs involving v
  . filter (not . null)
  . List.subsequences 
  . zip [0..]

-- TODO: improve brittleness of renaming etc.

quals :: Con -> [(Int,Base)] -> [Pred]
quals con = \case  
  [(_, TUnit)] -> [PTrue]  
  [(i, TInt)]  -> [PRel r | PRel (q1 (z i, TInt) -> Just r) <- universeBi con]
                  ++ [PRel $ EMod (EVar (z i)) (EInt 2 NoPV) :=: EInt 0 NoPV]
  [(i, b0)]    -> [PRel r | PRel (q1 (z i, b0) -> Just r) <- universeBi con]
  _            -> []
 where
  z i = Name ("z" <> Text.pack (show i)) NoPV

q1 :: (Name,Base) -> Rel -> Maybe Rel
q1 (z0,b0) = \case
  EVar _ :=: e | possible e -> Just $ EVar z0 :=: e
  EVar _ :≠: e | possible e -> Just $ EVar z0 :≠: e
  EVar _ :<: e | possible e -> Just $ EVar z0 :<: e
  EVar _ :≤: e | possible e -> Just $ EVar z0 :≤: e
  EVar _ :>: e | possible e -> Just $ EVar z0 :>: e
  EVar _ :≥: e | possible e -> Just $ EVar z0 :≥: e
  _                              -> Nothing
 where
  possible e = typeOfExpr e == Just b0 && null (freeVars e)
