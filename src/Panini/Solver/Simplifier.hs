module Panini.Solver.Simplifier where

import Algebra.Lattice
import Data.Generics.Uniplate.Operations
import Data.List.Extra qualified as List
import Data.Maybe
import Panini.Abstract.Semantics
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
    xs' = List.nubBy (==) $ concatMap flatAnd xs
    flatAnd = \case
      PAnd ys -> ys
      PTrue   -> []
      y       -> [y]

  PNot PTrue -> Just PFalse
  PNot PFalse -> Just PTrue
  PNot (PRel r) -> Just $ PRel $ inverse r 

  PRel r -> case normRel r of
    r' | r' == taut -> Just PTrue
       | r' == cont -> Just PFalse
       | r' /= r, all isConcrete (universeBi r') -> Just $ PRel r'
       | otherwise  -> Nothing

  PIff p PTrue -> Just p
  PIff PTrue p -> Just p
  PIff p PFalse -> Just $ PNot p
  PIff PFalse p -> Just $ PNot p

  PExists x _ p | x `notElem` freeVars p -> Just p

  -- ∃x:b. … ∧ x = y ∧ …   ≡   ∃x:b. P[x/y]
  PExists x1 b p0@(PAnd ps0) | Just y <- findAssignment ps0
    -> Just $ PExists x1 b $ subst y x1 p0
   where
    findAssignment (PRel (e1 :=: e2) : _ ) 
      | EVar x2 <- e1, x1 == x2, x1 `notFreeIn` e2 = Just e2
      | EVar x2 <- e2, x1 == x2, x1 `notFreeIn` e1 = Just e1      
    findAssignment (_                : ps)         = findAssignment ps
    findAssignment                     []          = Nothing

  -- ∃x:ℤ. x ⋈ c   ≡   ⊤    where c ∈ ℤ, ⋈ ∈ {=,≠,>,≥,<,≤}
  PExists x1 TInt (PRel (Rel _ (EVar x2) (EInt _ _)))
    | x1 == x2 -> Just PTrue

  -- ∃x:b. y = x   ≡   ⊤
  PExists x1 _ (PRel (y :=: EVar x2))
    | x1 == x2, x1 `notElem` freeVars y
    -> Just PTrue

  -- ∃x:b. x = y   ≡   ⊤
  PExists x1 _ (PRel (EVar x2 :=: y))
    | x1 == x2, x1 `notElem` freeVars y
    -> Just PTrue

  _ -> Nothing

-------------------------------------------------------------------------------

simplifyDNF :: [[Rel]] -> [[Rel]]
simplifyDNF = go []
 where
  go ys []     = ys
  go ys (x:xs) = case filter (/= taut) $ List.nub $ map normRel x of
    x' | null x'          -> [[]]
       | any (== cont) x' -> go ys xs
       | otherwise        -> go (x:ys) xs


-------------------------------------------------------------------------------

simplifyType :: Type -> Type
simplifyType = \case
  TBase x b Unknown   pv -> TBase x b Unknown pv
  TBase x b (Known p) pv -> TBase x b (Known $ simplifyPred p) pv
  TFun x s t pv          -> TFun x (simplifyType s) (simplifyType t) pv
