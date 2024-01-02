module Panini.Solver.Simplifier (simplify) where

import Data.Generics.Uniplate.Operations
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude

simplify :: Con -> Con
simplify = rewrite $ \case
  CAnd (CHead PTrue) c2 -> Just c2
  CAnd c1 (CHead PTrue) -> Just c1
  CAll _ _ _ (CHead PTrue) -> Just (CHead PTrue)

  CAll x TBool (PIff (PRel (EVar x1 :=: EBool True _)) p1)
    (CAll y TBool (PAnd [PIff (PRel (EVar y1 :=: EBool True _)) p2, PRel (EVar y2 :=: EVar x2)])
      (CHead (PRel (EVar y3 :=: EBool True _))))
    | x == x1, x `notElem` freeVars p1, y == y1, y `notElem` freeVars p2, p1 == p2, x == x2, y == y2, y == y3 -> Just $ CHead p1

  CAll x b p c | p' <- simplifyPred p, p' /= p -> Just $ CAll x b p' c
  CHead p      | p' <- simplifyPred p, p' /= p -> Just $ CHead p

  _ -> Nothing

simplifyPred :: Pred -> Pred
simplifyPred = rewrite $ \case
  POr xs
    | [x] <- xs      -> Just x
    | elem PTrue xs  -> Just PTrue
    | elem PFalse xs -> Just $ POr $ filter (/= PFalse) xs

  PAnd xs
    | [x] <- xs      -> Just x
    | elem PFalse xs -> Just PFalse
    | elem PTrue xs  -> Just $ PAnd $ filter (/= PTrue) xs

  PRel (x1 :=: x2) | x1 == x2 -> Just PTrue
  PRel (x1 :≠: x2) | x1 == x2 -> Just PFalse

  PExists x _ (PAnd [PRel (a :=: b), PRel (c :=: d)])
    | a == EVar x, b /= EVar x, c /= EVar x, d == EVar x -> Just $ PRel $ b :=: c
    | a == EVar x, b /= EVar x, c == EVar x, d /= EVar x -> Just $ PRel $ b :=: d
    | a /= EVar x, b == EVar x, c /= EVar x, d == EVar x -> Just $ PRel $ a :=: c
    | a /= EVar x, b == EVar x, c == EVar x, d /= EVar x -> Just $ PRel $ a :=: d

  _ -> Nothing
