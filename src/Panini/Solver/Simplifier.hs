module Panini.Solver.Simplifier where

import Algebra.Lattice
import Data.Generics.Uniplate.Operations
import Data.List.Extra qualified as List
import Data.Maybe
import Panini.Abstract.AInt (Inf(..))
import Panini.Abstract.AInt qualified as AInt
import Panini.Provenance
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

simplifyCon :: Con -> Con
simplifyCon = rewrite $ \case
  CAnd (CHead PTrue) c2 -> Just c2
  CAnd c1 (CHead PTrue) -> Just c1
  CAll _ _ _ (CHead PTrue) -> Just (CHead PTrue)

  CAnd (CHead p1) (CAnd (CHead p2) c) -> Just $ CHead (p1 ∧ p2) ∧ c
  CAnd (CHead p1) (CHead p2)          -> Just $ CHead (p1 ∧ p2)

  CAll x _ PTrue c | x `notElem` freeVars c -> Just c

  -- ∀x:b. φ ⇒ φ ∧ ψ   ≡   ∀x:b. φ ⇒ ψ
  CAll x b p (leftmostAnd -> (CHead q, c)) | p == q 
    -> Just $ maybe CTrue (CAll x b p) c

  -- ∀x:b. P(x) ⇒ (∀y:b. P(y) ∧ y = x ⇒ Q(y)) ∧ ψ   ≡   ∀x:b. P(x) ⇒ Q(x) ∧ ψ
  CAll x b1 p1 (leftmostAnd -> (CAll y b2 (PAnd [p2,r]) (CHead q2), c))
    | b1 == b2
    , p1 == subst (EVar x) y p2
    , r  == PRel (EVar y :=: EVar x)
    , let q1 = subst (EVar x) y q2
    -> Just $ CAll x b1 p1 $ CAnd (CHead q1) (fromMaybe CTrue c)

  -- ∀x:𝔹. (x = true ⇔ φ) ⇒ x = true ∧ ψ   ≡   φ ∧ ψ
  CAll x TBool (PIff p1 q) (leftmostAnd -> (CHead p2, c))
    | PRel (EVar x1 :=: EBool True _) <- p1
    , x1 == x, p1 == p2
    , x `notElem` freeVars q
    , x `notElem` maybe mempty freeVars c
    -> Just $ CAnd (CHead q) (fromMaybe CTrue c)

  CAll x b p c | p' <- simplifyPred p, p' /= p -> Just $ CAll x b p' c
  CHead p      | p' <- simplifyPred p, p' /= p -> Just $ CHead p'

  _ -> Nothing

leftmostAnd :: Con -> (Con, Maybe Con)
leftmostAnd = \case
  CAnd (CAnd c1 c2) c3 -> leftmostAnd (CAnd c1 (CAnd c2 c3))
  CAnd c c3            -> (c, Just c3)
  c                    -> (c, Nothing)

-------------------------------------------------------------------------------

simplifyPred :: Pred -> Pred
simplifyPred = rewrite $ \case
  POr xs
    | null xs        -> Just PFalse
    | [x] <- xs      -> Just x
    | elem PTrue xs  -> Just PTrue
    | elem PFalse xs -> Just $ POr $ filter (/= PFalse) xs

  PAnd xs
    | null xs        -> Just PTrue
    | [x] <- xs      -> Just x
    | elem PFalse xs -> Just PFalse
    | xs' /= xs      -> Just $ PAnd xs'
   where
    xs' = List.nubBy symRelEq $ concatMap flatAnd xs
    flatAnd = \case
      PAnd ys -> ys
      PTrue   -> []
      y       -> [y]

  PNot PTrue -> Just PFalse
  PNot PFalse -> Just PTrue
  PNot (PRel r) -> Just $ PRel $ inverse r 

  PRel r -> case simplifyRel r of
    r' | isTaut r' -> Just PTrue
       | isCont r' -> Just PFalse
       | r' /= r   -> Just $ PRel r'
       | otherwise -> Nothing

  PIff p PTrue -> Just p
  PIff PTrue p -> Just p
  PIff p PFalse -> Just $ PNot p
  PIff PFalse p -> Just $ PNot p

  -- ∃x:b. P(x) ∧ y = x   ≡   P(y)
  PExists x1 _ (PAnd (List.unsnoc -> Just (xs, PRel (y :=: EVar x2)))) 
    | x1 == x2, x1 `notElem` freeVars y
    -> Just $ meets $ map (subst y x1) xs
  
  -- ∃x:b. y = x   ≡   ⊤
  PExists x1 _ (PRel (y :=: EVar x2))
    | x1 == x2, x1 `notElem` freeVars y
    -> Just PTrue

  _ -> Nothing

symRelEq :: Pred -> Pred -> Bool
symRelEq (PRel r1) (PRel r2) | r1 == r2                           = True
                             | Just r1' <- converse r1, r1' == r2 = True
                             | otherwise                          = False
symRelEq p1        p2                                             = p1 == p2

-------------------------------------------------------------------------------

simplifyType :: Type -> Type
simplifyType = \case
  TBase x b Unknown   pv -> TBase x b Unknown pv
  TBase x b (Known p) pv -> TBase x b (Known $ simplifyPred $ normReft x p) pv
  TFun x s t pv          -> TFun x (simplifyType s) (simplifyType t) pv

normReft :: Name -> Pred -> Pred
normReft v = \case
  PAnd xs   -> PAnd (map rearrangeRel xs)
  POr xs    -> POr (map rearrangeRel xs)
  PImpl p q -> PImpl (normReft v p) (normReft v q)
  PIff p q  -> PIff (normReft v p) (normReft v q)
  PNot p    -> PNot (normReft v p)
  p         -> p
 where
  rearrangeRel (PRel r@(Rel _ e1 e2)) 
    | v `notElem` freeVars e1, v `elem` freeVars e2
    , Just r' <- converse r = PRel r'
  rearrangeRel p = p

-------------------------------------------------------------------------------

simplifyRel :: Rel -> Rel
simplifyRel r = case normRel r of
  EVar x1 :=: EVar x2 | x1 == x2 -> taut
  EVar x1 :≠: EVar x2 | x1 == x2 -> cont
  ECon c1 :=: ECon c2 -> if c1 == c2 then taut else cont
  ECon c1 :≠: ECon c2 -> if c1 == c2 then cont else taut
  
  -- TODO: we assume here that all Rel are typed correctly!
  EVar _ :=: ECon (U _) -> taut
  ECon (U _) :=: EVar _ -> taut
  EVar _ :≠: ECon (U _) -> cont
  ECon (U _) :≠: EVar _ -> cont
  
  EVar x :≠: EBool b pv -> EVar x :=: EBool (not b) pv
  EBool b pv :≠: EVar x -> EBool (not b) pv :=: EVar x
  EInt a _ :>: EInt b _ -> if a >  b then taut else cont
  EInt a _ :≥: EInt b _ -> if a >= b then taut else cont
  EInt a _ :<: EInt b _ -> if a <  b then taut else cont
  EInt a _ :≤: EInt b _ -> if a <= b then taut else cont

  (e1 :-: EIntA a) :<: e2 | e1 == e2, AInt.minimum a >  Just (Fin 0) -> taut
  (e1 :-: EIntA a) :≤: e2 | e1 == e2, AInt.minimum a >= Just (Fin 0) -> taut
  (e1 :-: EIntA a) :>: e2 | e1 == e2, AInt.minimum a >= Just (Fin 0) -> cont
  (e1 :-: EIntA a) :≥: e2 | e1 == e2, AInt.minimum a >  Just (Fin 0) -> cont

  r' -> r'
 where
  taut = EBool True NoPV :=: EBool True NoPV
  cont = EBool True NoPV :≠: EBool True NoPV
