module Panini.Solver.Simplifier (simplify, simplifyCon, simplifyPred, simplifyType) where

import Data.Generics.Uniplate.Operations
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude
import Panini.Monad

simplify :: Con -> Pan Con
simplify c = do
  logMessage "Simplify constraint"
  let c' = simplifyCon c
  logData c'
  return c'

simplifyCon :: Con -> Con
simplifyCon = rewrite $ \case
  CAnd (CHead PTrue) c2 -> Just c2
  CAnd c1 (CHead PTrue) -> Just c1
  CAll _ _ _ (CHead PTrue) -> Just (CHead PTrue)

  CAll x _ PTrue c | x `notElem` freeVars c -> Just c

  -- The common pattern
  --       ∀x:𝔹. (x = true ⇔ φ) ⇒ (∀y:𝔹. (y = true ⇔ φ) ∧ y = x ⇒ y = true) ∧ ψ
  -- can be simplified to
  --       φ ∧ ψ
  CAll x TBool (PIff (PRel (EVar x1 :=: EBool True _)) p1) cs
    | x == x1, x `notElem` freeVars p1
    , (c1,c2) <- leftmostAnd cs
    , (CAll y TBool (PAnd [ PIff (PRel (EVar y1 :=: EBool True _)) p2
                          , PRel (EVar y2 :=: EVar x2)])
                    (CHead (PRel (EVar y3 :=: EBool True _)))) <- c1
    , y == y1, y `notElem` freeVars p2
    , p1 == p2, x == x2, y == y2, y == y3
    -> case c2 of
      Nothing                    -> Just $ CHead p1
      Just q 
        | x `notElem` freeVars q -> Just $ CAnd (CHead p1) q
        | otherwise              -> Just $ CAll x TBool p1 q
   where
    leftmostAnd = \case
      CAnd (CAnd c1 c2) c3 -> leftmostAnd (CAnd c1 (CAnd c2 c3))
      CAnd c c3            -> (c, Just c3)
      c                    -> (c, Nothing)

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


  PRel (EVar x1 :=: EVar x2) | x1 == x2 -> Just PTrue
  PRel (EVar x1 :≠: EVar x2) | x1 == x2 -> Just PFalse

  PRel (ECon c1 :=: ECon c2) -> Just $ if c1 == c2 then PTrue else PFalse
  PRel (ECon c1 :≠: ECon c2) -> Just $ if c1 == c2 then PFalse else PTrue

  PNot PTrue -> Just PFalse
  PNot PFalse -> Just PTrue

  PNot (PRel r) -> Just $ PRel $ inverse r 

  PRel (EVar x :≠: EBool b pv) -> Just $ PRel (EVar x :=: EBool (not b) pv)

  PIff p PTrue -> Just p
  PIff PTrue p -> Just p
  PIff p PFalse -> Just $ PNot p
  PIff PFalse p -> Just $ PNot p

  PExists x _ (PAnd [PRel (a :=: b), PRel (c :=: d)])
    | a == EVar x, b /= EVar x, c /= EVar x, d == EVar x -> Just $ PRel $ b :=: c
    | a == EVar x, b /= EVar x, c == EVar x, d /= EVar x -> Just $ PRel $ b :=: d
    | a /= EVar x, b == EVar x, c /= EVar x, d == EVar x -> Just $ PRel $ a :=: c
    | a /= EVar x, b == EVar x, c == EVar x, d /= EVar x -> Just $ PRel $ a :=: d

  _ -> Nothing

simplifyType :: Type -> Type
simplifyType = \case
  TBase x b Unknown   pv -> TBase x b Unknown pv
  TBase x b (Known p) pv -> TBase x b (Known $ simplifyPred p) pv
  TFun x s t pv          -> TFun x (simplifyType s) (simplifyType t) pv
