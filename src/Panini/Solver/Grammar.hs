module Panini.Solver.Grammar (infer) where

import Panini.Syntax
import Panini.Algebra.Lattice
import Prelude
import Panini.Abstract.Interpretation
import Panini.Solver.Grammar.Rewriting
import Panini.Abstract.AString
import Panini.Abstract.AValue
import Panini.Pretty.Printer
import Data.Foldable


-------------------------------------------------------------------------------

infer :: Name -> Con -> Pred
infer s = PPred . concretizeVar s . PAbs . AString
        . foldl' (∨) (⊥) 
        . map (foldl' (∧) (⊤) . map (abstractStringVar s))  -- TODO
        . toPredsDNF
        . rewrite

-- TODO: either make this unnecessary or deal with errors gracefully
abstractStringVar :: Name -> Pred2 -> AString
abstractStringVar x p = case abstractVar x TString p of
  PAbs (AString s) -> s
  _                -> error "expected abstract string"

-------------------------------------------------------------------------------

-- TODO: ensure PRel at bottom
toPredsDNF :: Pred -> [[Pred2]]
toPredsDNF (POr ps) | all isPAnd ps = [[y | PPred y <- xs] | PAnd xs <- ps]
toPredsDNF (PAnd xs) | all isPRel xs = [[y | PPred y <- xs]]
toPredsDNF c = error $ "expected (POr [PAnd _]) instead of " ++ showPretty c
