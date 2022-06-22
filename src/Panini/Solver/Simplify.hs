-- TODO: module documentation
module Panini.Solver.Simplify where

import Panini.Syntax
import Panini.Provenance
import Panini.Substitution
import Prelude

class Simplifable a where
  simplify :: a -> a

instance Simplifable Pred where
  -- TODO: preserve provenance information
  simplify = \case
    PAnd (simplifyAnd -> ps)
      | []  <- ps -> PTrue NoPV
      | [p] <- ps -> p
      | otherwise -> PAnd ps
    
    PDisj (simplify -> p) (simplify -> q)
      | PFalse _ <- p -> q
      | PFalse _ <- q -> p
      --  | PTrue _  <- p -> PTrue NoPV  -- TODO: information loss?
      --  | PTrue _  <- q -> PTrue NoPV  -- TODO: information loss?
      | otherwise     -> PDisj p q

    PNot (simplify -> p) -> PNot p

    PImpl (simplify -> p) (simplify -> q)
      | PTrue _ <- p -> q
      | PTrue _ <- q -> PTrue NoPV
      | otherwise    -> PImpl p q
  
    PIff (simplify -> p) (simplify -> q)
      | PTrue _ <- p -> q
      | PTrue _ <- q -> p
      | otherwise    -> PIff p q

    PExists x b (simplify -> p)      
      | trivialExists x b p -> PTrue NoPV  -- TODO: information loss?
      | x `elem` freeVars p -> PExists x b p      
      | otherwise           -> p
  
    PBin o (simplify -> p1) (simplify -> p2) -> PBin o p1 p2
    PRel r (simplify -> p1) (simplify -> p2) -> PRel r p1 p2
    PFun f (map simplify -> ps)              -> PFun f ps

    p -> p

--TODO: this is a hack; depending on the sort of x, it might not even be sound!
trivialExists :: Name -> Base -> Pred -> Bool
trivialExists x _ = \case
  PRel Eq (PVal (V v1)) (PVal (V v2)) -> v1 == x || v2 == x
  _                                   -> False

instance Simplifable Con where
  simplify = \case
    CHead (simplify -> p) -> CHead p
    CAnd (simplify -> c1) (simplify -> c2)
      | CHead (PTrue _) <- c1 -> c2
      | CHead (PTrue _) <- c2 -> c1
      | otherwise     -> CAnd c1 c2
    CAll x b (simplify -> p) (simplify -> c)
      | CHead (PTrue _) <- c -> CHead (PTrue NoPV)
      | PTrue _ <- p, x `notElem` (freeVars p ++ freeVars c) -> CAll x b p c
      | otherwise -> CAll x b p c

simplifyAnd :: [Pred] -> [Pred]
simplifyAnd = go []
  where
    go qs [] = reverse qs
    go qs ((simplify -> p) : ps)
      | PFalse _ <- p = [p]
      | taut p        = go qs ps
      | otherwise     = go (p:qs) ps

taut :: Pred -> Bool
taut (PTrue _)      = True
taut (PAnd [])      = True
taut (PAnd ps)      = all taut ps
taut (PRel Eq  p q) = p == q
taut (PRel Leq p q) = p == q
taut (PRel Geq p q) = p == q
taut _              = False
