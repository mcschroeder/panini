-- TODO: module documentation
module Panini.Solver.Simplify where

import Panini.Syntax
import Prelude

class Simplifable a where
  simplify :: a -> a

instance Simplifable Pred where
  -- TODO: preserve provenance information
  simplify = \case
    PAnd (simplifyAnd -> ps)
      | []  <- ps -> PTrue
      | [p] <- ps -> p
      | otherwise -> PAnd ps
    
    POr (simplifyOr -> ps)
      | []  <- ps -> PFalse
      | [p] <- ps -> p
      | otherwise -> POr ps
    
    PNot (simplify -> p) -> PNot p

    PImpl (simplify -> p) (simplify -> q)
      | PTrue <- p -> q
      | PTrue <- q -> PTrue
      | otherwise  -> PImpl p q
  
    PIff (simplify -> p) (simplify -> q)
      | PTrue <- p -> q
      | PTrue <- q -> p
      | otherwise  -> PIff p q

    PExists x b (simplify -> p)      
      | trivialExists x b p -> PTrue  -- TODO: information loss?
      | x `elem` freeVars p -> PExists x b p      
      | otherwise           -> p
  
    p -> p

--TODO: this is a hack; depending on the sort of x, it might not even be sound!
trivialExists :: Name -> Base -> Pred -> Bool
trivialExists x _ = \case
  PRel Eq (PVar v1) (PVar v2) -> v1 == x || v2 == x
  _                           -> False

instance Simplifable Con where
  simplify = \case
    CHead (simplify -> p) -> CHead p
    CAnd (simplify -> c1) (simplify -> c2)
      | CTrue <- c1 -> c2
      | CTrue <- c2 -> c1
      | otherwise   -> CAnd c1 c2
    CAll x b (simplify -> p) (simplify -> c)
      | CTrue <- c -> CTrue
      | PTrue <- p, x `notElem` (freeVars p ++ freeVars c) -> CAll x b p c
      | otherwise -> CAll x b p c

simplifyAnd :: [Pred] -> [Pred]
simplifyAnd = go []
  where
    go qs [] = reverse qs
    go qs ((simplify -> p) : ps)
      | PFalse <- p = [p]
      | taut p      = go qs ps
      | otherwise   = go (p:qs) ps

simplifyOr :: [Pred] -> [Pred]
simplifyOr = go []
  where
    go qs [] = reverse qs
    go qs ((simplify -> p) : ps)
      | PTrue  <- p = [p]
      | PFalse <- p = go qs ps
      | otherwise   = go (p:qs) ps

taut :: Pred -> Bool
taut PTrue         = True
taut (PAnd [])     = True
taut (PAnd ps)     = all taut ps
taut (PRel Eq p q) = p == q
taut (PRel Le p q) = p == q
taut (PRel Ge p q) = p == q
taut _             = False
