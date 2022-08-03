{-# LANGUAGE OverloadedStrings #-}

module Panini.Solver.Grammar (solve) where

import Panini.Syntax
import Prelude
--import Data.Map qualified as Map

solve :: Con -> Pred
solve = go
  where
    go (CHead p) = norm p
    go (CAnd (CAll _ TUnit p1@(PVar x) q1) (CAll _ TUnit p2@(PNot (PVar y)) q2))
      | x == y = (p1 `pAnd` go q1) `pOr` (p2 `pAnd` go q2)
    go (CAnd c1 c2) = go c1 `pAnd` go c2
    go (CAll _ _ p c) = norm p `pAnd` go c

-- solve = snd . goC Map.empty
--   where
--     goC ks (CHead p) = goP ks p
    
--     goC ks (CAnd (CAll _ TUnit p1@(PVar x) q1) (CAll _ TUnit p2@(PNot (PVar y)) q2))
--       | x == y = 
--         let (ks1,p1') = goP ks p1
--             (_  ,q1') = goC ks1 q1
--             (ks2,p2') = goP ks p2
--             (_  ,q2') = goC ks2 q2
--         in (ks, (p1' `pAnd` q1') `pOr` (p2' `pAnd` q2'))
    
--     goC ks (CAnd c1 c2) = 
--       let (ks1,p1) = goC ks c1
--           (ks2,p2) = goC ks1 c2
--       in (ks2, p1 `pAnd` p2)
    
--     goC ks (CAll _ _ p c) = 
--       let (ks1,p1) = goP ks p
--           (ks2,p2) = goC ks1 c
--       in (ks2, p1 `pAnd` p2)

--     goP ks (PNot (PVar x)) = case Map.lookup x ks of
--       Just (PIff (PVar x') q) | x == x' -> (ks, norm (PNot q))
--       _ -> (ks, PNot (PVar x))

--     goP ks (PVar x) = case Map.lookup x ks of
--       Just (PIff (PVar x') q) | x == x' -> (ks, q)
--       _ -> (ks, PVar x)
    
--     goP ks (norm -> p) = case p of
--       PIff (PVar x) _ -> (Map.insert x p ks, p)
--       PRel _ (PVar x) _ -> (Map.insert x p ks, p)
--       _ -> (ks, p)

norm :: Pred -> Pred
norm (PNot p) = case p of
  PCon (B b _) -> PCon (B (not b) NoPV)
  PRel r x y -> norm $ PRel (inverse r) x y
  _ -> PNot p

norm (PRel r (PCon c) (PVar x))    = PRel (converse r) (PVar x)    (PCon c)
norm (PRel r (PCon c) (PFun f ps)) = PRel (converse r) (PFun f ps) (PCon c)

norm (PAnd ps) = PAnd $ map norm ps
norm (POr  ps) = POr  $ map norm ps

norm p = p

inverse :: Rel -> Rel
inverse = \case
  Eq -> Neq
  Neq -> Eq
  Geq -> Lt
  Leq -> Gt
  Gt -> Leq
  Lt -> Geq

converse :: Rel -> Rel
converse = \case
  Eq  -> Eq
  Neq -> Neq
  Geq -> Leq
  Leq -> Geq
  Gt  -> Lt
  Lt  -> Gt
