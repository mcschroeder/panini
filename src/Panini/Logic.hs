module Panini.Logic
  ( solve
  , module Panini.Logic.Assignment
  ) where

import Control.Applicative
import Control.Monad
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Panini.Logic.Assignment
import Panini.Logic.Constraints
import Panini.Logic.Fusion qualified as Fusion
import Panini.Logic.Grammar qualified as Grammar
import Panini.Logic.Liquid qualified as Liquid
import Panini.Logic.Simplify
import Panini.Monad
import Panini.Pretty.Printer
import Panini.Syntax
import Prelude

-- TODO: be strict in each of these steps

solve :: Con -> Pan Assignment
solve c0 = do
  -- logMessage "Solver" "Simplify constraint"
  let !c1 = c0 --simplifyCon c0
  -- logData c1

  logMessage "Grammar" "Find grammar variables"
  let !ks_gram = Set.fromList $ grammarVars c1
  logData "Grammar Variables" ks_gram

  c2 <- Fusion.solve ks_gram c1

  logMessage "Solver" "Simplify constraint"
  let !c3 = simplifyCon c2 -- TODO: disable this and make it work regardless
  logData "Simplified Constraint" c3
  --let !c3 = c2

  logMessage "Grammar" "Find grammar consequents"
  let !cs = Map.fromList
         $ map (\(k,c) -> (k, fromJust c)) 
         $ filter (isJust . snd) 
         $ zip (Set.toAscList ks_gram)
         $ map (grammarConsequent c3) 
         $ Set.toAscList ks_gram
  logData "Grammar Consequents" cs

  let solveOne gs (k,c) = do
        logMessageDoc "Grammar" $ "Solve grammar variable" <+> pretty k
        let c' = apply gs c
        logData "Current Consequent" c'
        let !g = Grammar.infer "z0" c'  -- TODO: generalize for variable name
        logData "Grammar Solution" g
        return $ Map.insert k g gs

  !s_grammar <- foldM solveOne mempty (Map.toAscList cs)

  logMessage "Grammar" "Apply grammar solution"
  let !c4 = apply s_grammar c3
  logData "Constraint w/ Grammar Solution Applied" c4

  -- logMessage "Solver" "Simplify constraint"
  let !c5 = c4 --simplifyCon c4
  -- logData c5

  logMessage "Liquid" "Compute approximate solutions for residuals"
  !s_liquid <- Liquid.solve c5 []
  logData "solution" s_liquid

  let s_final = Map.unions [s_grammar, s_liquid]
  logData "Final Solution" s_final
  
  return s_final

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

