{-# LANGUAGE OverloadedStrings #-}

module Panini.Solver.Grammar (solve) where

import Panini.Syntax
--import Panini.Solver.Simplify
import Prelude

solve :: Con -> Pred
solve = PAnd . go [] . mapCon norm
  where
    go k (CHead p)      = resolve k p
    go k (CAnd c1 c2)   = let k' = go k c1 in go k' c2
    go k (CAll _ _ p c) = let k' = resolve k p in go k' c


resolve :: [Pred] -> Pred -> [Pred]
resolve ks p = p:ks

-- resolveAll :: Pred -> Pred -> Pred
-- resolveAll (PAnd ks) (norm -> p) = PAnd $ map (flip resolve p) ks
-- resolveAll (POr  ks) (norm -> p) = POr  $ map (flip resolve p) ks
-- resolveAll       k   (norm -> p) = resolve k p

-- resolve :: Pred -> Pred -> Pred
-- resolve (PRel Eq s@(PFun "charat" _) (PVar x1))  -- s[i] = x  ─┐
--         (PRel Eq (PVar x2) c@(PCon (S _ _)))     -- x = c     ─┤
--         | x1 == x2 = PRel Eq s c                 -- s[i] = c ◀─┘

-- -- p1 <==> p2 ─▶ (p1 ∧ p2) ∨ (¬p1 ∧ ¬p2)
-- resolve k (PIff p1 p2)
--         = (     p1 `pAnd` resolveAll k       p1) `pOr` 
--           (PNot p2 `pAnd` resolveAll k (PNot p1))

-- resolve k p = k `pAnd` p

norm :: Pred -> Pred
norm (PRel Eq (PVar x) s@(PFun "charat" _))  -- x = s[i]  ─┐
    = PRel Eq s (PVar x)                     -- s[i] = x ◀─┘

norm (PRel r x s@(PFun "len" _)) --- x < |s|  ─┐
    = PRel (invRel r) s x        --- |s| > x ◀─┘

norm (PRel r c@(PCon _) x@(PVar _))  -- c < x  ─┐
    = PRel (invRel r) x c            -- x > c ◀─┘

norm (PNot (PRel Eq p1 p2))  -- ¬(x = y)  ─┐
    = PRel Neq p1 p2         --   x ≠ y  ◀─┘

norm p = p

invRel :: Rel -> Rel
invRel = \case
  Eq  -> Eq
  Neq -> Neq
  Geq -> Leq
  Leq -> Geq
  Gt  -> Lt
  Lt  -> Gt

mapCon :: (Pred -> Pred) -> Con -> Con
mapCon f = \case
  CHead p -> CHead (mapPred f p)
  CAnd c1 c2 -> CAnd (mapCon f c1) (mapCon f c2)
  CAll x b p c -> CAll x b (mapPred f p) (mapCon f c)

mapPred :: (Pred -> Pred) -> Pred -> Pred
mapPred f = \case
  PBin o p1 p2 -> f $ PBin o (mapPred f p1) (mapPred f p2)
  PRel r p1 p2 -> f $ PRel r (mapPred f p1) (mapPred f p2)
  PAnd ps -> f $ PAnd (map (mapPred f) ps)
  POr ps -> f $ POr (map (mapPred f) ps)
  PImpl p q -> f $ PImpl (mapPred f p) (mapPred f q)
  PIff p q -> f $ PIff (mapPred f p) (mapPred f q)
  PNot p -> f $ PNot (mapPred f p)
  PFun x ps -> f $ PFun x (map (mapPred f) ps)
  PExists x b p -> f $ PExists x b (mapPred f p)
  p -> f p
