module Panini.Solver (solve) where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Panini.Logger
import Panini.Monad
import Panini.Solver.Assignment
import Panini.Solver.Fusion qualified as Fusion
import Panini.Solver.Grammar qualified as Grammar
import Panini.Solver.Liquid qualified as Liquid
import Panini.Solver.Simplify
import Panini.Syntax
import Prelude
import Control.Applicative
import Data.Maybe

-- TODO: be strict in each of these steps

solve :: Con -> Pan (Maybe Assignment)
solve c0 = do
  logMessage Info "Solver" "Simplify constraint"
  let !c1 = simplifyCon c0
  logData Trace c1

  logMessage Info "Fusion" "Find cut variables"
  let !ks_cut = Fusion.cutVars c1
  logData Trace ks_cut

  logMessage Info "Grammar" "Find grammar variables"
  let !ks_gram = grammarVars c1
  logData Trace ks_gram

  logMessage Info "Fusion" "Compute exact solutions for other variables"
  let !ks = kvars c1
  let !ks' = ks Set.\\ ks_cut Set.\\ (Set.fromList ks_gram)
  logData Trace ks'
  let !c2 = Fusion.elim (Set.toList ks') c1
  logData Trace c2

  logMessage Info "Solver" "Simplify constraint"
  let !c3 = simplifyCon c2
  logData Trace c3

  logMessage Info "Grammar" "Find grammar consequents"
  let !cs = Map.fromList
         $ map (\(k,c) -> (k, fromJust c)) 
         $ filter (isJust . snd) 
         $ zip ks_gram 
         $ map (grammarConsequent c3) ks_gram
  logData Trace cs

  logMessage Info "Grammar" "Solve grammar variables"
  let !gs = Map.map Grammar.solve cs
  logData Debug gs

  logMessage Info "Grammar" "Apply grammar solution"
  let !c4 = apply gs c3
  logData Trace c4

  logMessage Info "Solver" "Simplify constraint"
  let !c5 = simplifyCon c4
  logData Trace c5

  logMessage Info "Liquid" "Compute approximate solutions for residuals"
  !s <- Liquid.solve c5 []
  logData Trace s

  return s

grammarVars :: Con -> [KVar]
grammarVars = go
  where
    go (CHead _) = []
    go (CAnd c1 c2) = go c1 ++ go c2
    go (CAll x TString (PAppK k@(KVar _ [TString]) [Var y]) _) | y == x = [k]
    go (CAll _ _ _ c) = go c

grammarConsequent :: Con -> KVar -> Maybe Con
grammarConsequent c0 k0 = go c0
  where
    go (CHead _) = Nothing
    go (CAnd c1 c2) = go c1 <|> go c2
    go (CAll x TString (PAppK k [Var y]) c) | k0 == k, x == y = Just c'
      where
        -- TODO
        -- IMPORTANT: we need to substitute the bound variables with generic kvar params
        -- so that later on we can apply without problems
        c' = substN (map Var $ kparams k) [y] c
    go (CAll _ _ _ c) = go c

