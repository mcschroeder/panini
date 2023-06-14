module Panini.Solver.Grammar (infer) where

import Panini.Syntax
import Panini.Algebra.Lattice
import Prelude
import Panini.Solver.Grammar.Abstract
import Panini.Solver.Grammar.Rewriting
import Panini.Solver.Abstract.AString
import Panini.Pretty.Printer
import Data.Foldable


-------------------------------------------------------------------------------

infer :: Name -> Con -> Pred
infer s = concretizeVar s . PAbs . AString
        . foldl' (∨) (⊥) 
        . map (foldl' (∧) (⊤) . map (abstractStringVar s))  -- TODO
        . toPredsDNF
        . rewrite

-- TODO: either make this unnecessary or deal with errors gracefully
abstractStringVar :: Name -> Pred -> AString
abstractStringVar x p = case abstractVar x TString p of
  PAbs (AString s) -> s
  _                -> error "expected abstract string"

-------------------------------------------------------------------------------

-- TODO: ensure PRel at bottom
toPredsDNF :: Pred -> [[Pred]]
toPredsDNF (POr ps) | all isPAnd ps = [xs | PAnd xs <- ps]
toPredsDNF (PAnd xs) | all isPRel xs = [xs]
toPredsDNF c = error $ "expected (POr [PAnd _]) instead of " ++ showPretty c
