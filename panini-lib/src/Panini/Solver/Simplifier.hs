module Panini.Solver.Simplifier where

import Algebra.Lattice
import Data.Generics.Uniplate.Operations
import Data.List.Extra qualified as List
import Data.Maybe
import Panini.Abstract.Semantics
import Panini.Abstract.AInt qualified as AInt
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude
import Panini.Abstract.AValue (APred)
import Panini.Panic

-------------------------------------------------------------------------------

class Simplifiable a where
  simplify :: a -> a

-------------------------------------------------------------------------------

instance Simplifiable Con where
  simplify = rewrite simplifyCon

simplifyCon :: Con -> Maybe Con
simplifyCon = \case
  CAnd (CHead PTrue) c2 -> Just c2
  CAnd c1 (CHead PTrue) -> Just c1
  CAll _ _ _ (CHead PTrue) -> Just (CHead PTrue)

  CAnd (CHead PFalse) _ -> Just CFalse
  CAnd _ (CHead PFalse) -> Just CFalse

  CAnd (CHead p1) (CAnd (CHead p2) c) -> Just $ CHead (p1 ∧ p2) ∧ c
  CAnd (CHead p1) (CHead p2)          -> Just $ CHead (p1 ∧ p2)

  CAll x _ PTrue c | x `notElem` freeVars c -> Just c

  -- ∀x:b. φ ⇒ φ ∧ ψ   ≡   ∀x:b. φ ⇒ ψ
  CAll x b p (leftmostAnd -> (CHead q, c)) | p == q 
    -> Just $ maybe CTrue (CAll x b p) c

  -- ∀x:b. P(x) ⇒ (∀y:b. P(y) ∧ y = x ⇒ Q(y)) ∧ ψ   ≡   ∀x:b. P(x) ⇒ Q(x) ∧ ψ
  CAll x b1 p1 (leftmostAnd -> (CAll y b2 (PAnd [p2,r]) (CHead q2), c))
    | b1 == b2
    , p1 == subst (EVar x b1) y p2
    , r  == PRel (EVar y b2 :=: EVar x b1)
    , let q1 = subst (EVar x b1) y q2
    -> Just $ CAll x b1 p1 $ CAnd (CHead q1) (fromMaybe CTrue c)

  -- ∀x:𝔹. (x = true ⇔ φ) ⇒ x = true ∧ ψ   ≡   φ ∧ ψ
  CAll x TBool (PIff p1 q) (leftmostAnd -> (CHead p2, c))
    | PRel (EVar x1 TBool :=: EBool True _) <- p1
    , x1 == x, p1 == p2
    , x `notElem` freeVars q
    , x `notElem` maybe mempty freeVars c
    -> Just $ CAnd (CHead q) (fromMaybe CTrue c)
  
  -- (∀x:𝔹. x = false ⇒ x = true)   ≡   false
  CAll x TBool p (CHead q)
    | PRel (EVar x1 TBool :=: EBool v1 _) <- p
    , PRel (EVar x2 TBool :=: EBool v2 _) <- q
    , x == x1, x1 == x2, v1 /= v2
    -> Just CFalse

  CAll x b p c | p' <- simplify p, p' /= p -> Just $ CAll x b p' c
  CHead p      | p' <- simplify p, p' /= p -> Just $ CHead p'

  _ -> Nothing

leftmostAnd :: Con -> (Con, Maybe Con)
leftmostAnd = \case
  CAnd (CAnd c1 c2) c3 -> leftmostAnd (CAnd c1 (CAnd c2 c3))
  CAnd c c3            -> (c, Just c3)
  c                    -> (c, Nothing)

-------------------------------------------------------------------------------

instance Simplifiable Pred where
  simplify = rewrite simplifyPred

instance Simplifiable APred where
  simplify = rewrite simplifyPredA

-- TODO: consolidate different simplifyPreds
simplifyPredA :: APred -> Maybe APred
simplifyPredA = \case
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
      | EVar x2 _ <- e1, x1 == x2, x1 `notFreeIn` e2 = Just e2
      | EVar x2 _ <- e2, x1 == x2, x1 `notFreeIn` e1 = Just e1      
    findAssignment (_                : ps)         = findAssignment ps
    findAssignment                     []          = Nothing

  -- ∃x:b. y = x   ≡   ⊤
  PExists x1 _ (PRel (y :=: EVar x2 _))
    | x1 == x2, x1 `notElem` freeVars y
    -> Just PTrue

  -- ∃x:b. x = y   ≡   ⊤
  PExists x1 _ (PRel (EVar x2 _ :=: y))
    | x1 == x2, x1 `notElem` freeVars y
    -> Just PTrue

  _ -> Nothing


simplifyPred :: Pred -> Maybe Pred
simplifyPred = \case
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
  
  -- reduce chains of of simple integer relations via abstract interpretation,
  -- e.g., x > 0 ∧ x ≥ 1 ∧ φ ≡ x ≥ 1 ∧ φ
  p@(PAnd xs0@(PRel (Rel _ (EVar i _) (EInt _ _)) : _))
    | p' <- go top xs0, p' /= p -> Just p'
   where
    go v (PRel (Rel op (EVar i1 _) (EInt c _)) : xs) 
      | i1 == i = case op of
          Eq -> go (v ∧ AInt.eq c) xs
          Ne -> go (v ∧ AInt.ne c) xs
          Lt -> go (v ∧ AInt.lt c) xs
          Le -> go (v ∧ AInt.le c) xs
          Gt -> go (v ∧ AInt.gt c) xs
          Ge -> go (v ∧ AInt.ge c) xs
          _  -> impossible
    go v xs = meets $ concretizeInt i v : xs

  PAnd [PRel (EVar i1 _ :≥: EInt 0 _pv1), PRel (EVar i2 _ :>: EInt 0 pv2)]
    | i1 == i2 -> Just $ PRel (EVar i2 TInt :>: EInt 0 pv2)

  POr [PRel (EVar i1 _ :>: EInt 0 _pv1), PRel (EVar i2 _ :=: EInt 0 pv2)]
    | i1 == i2 -> Just $ PRel (EVar i2 TInt :≥: EInt 0 pv2)  

  POr [PRel (EVar i1 _ :≥: EInt 1 _pv1), PRel (EVar i2 _ :=: EInt 0 pv2)]
    | i1 == i2 -> Just $ PRel (EVar i2 TInt :≥: EInt 0 pv2)

  PNot PTrue -> Just PFalse
  PNot PFalse -> Just PTrue
  PNot (PRel r) -> Just $ PRel $ inverse r 

  PRel r -> case normRel r of
    Left True -> Just PTrue
    Left False -> Just PFalse
    Right r'
       | r' /= r -> Just $ PRel r'
       | otherwise -> Nothing

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
      | EVar x2 _ <- e1, x1 == x2, x1 `notFreeIn` e2 = Just e2
      | EVar x2 _ <- e2, x1 == x2, x1 `notFreeIn` e1 = Just e1      
    findAssignment (_                : ps)         = findAssignment ps
    findAssignment                     []          = Nothing

  -- ∃x:ℤ. x ⋈ c   ≡   ⊤    where c ∈ ℤ, ⋈ ∈ {=,≠,>,≥,<,≤}
  PExists x1 TInt (PRel (Rel _ (EVar x2 _) (EInt _ _)))
    | x1 == x2 -> Just PTrue

  -- ∃x:b. y = x   ≡   ⊤
  PExists x1 _ (PRel (y :=: EVar x2 _))
    | x1 == x2, x1 `notElem` freeVars y
    -> Just PTrue

  -- ∃x:b. x = y   ≡   ⊤
  PExists x1 _ (PRel (EVar x2 _ :=: y))
    | x1 == x2, x1 `notElem` freeVars y
    -> Just PTrue

  POr [PRel (EVar x1 TBool :=: EBool True _), PRel (EVar x2 TBool :=: EBool False _)]
    | x1 == x2
    -> Just PTrue

  _ -> Nothing

-------------------------------------------------------------------------------

instance Simplifiable Type where
  simplify = \case
    TBase x b Unknown   pv -> TBase x b Unknown pv
    TBase x b (Known p) pv -> TBase x b (Known $ simplify p) pv
    TFun x s t pv          -> TFun x (simplify s) (simplify t) pv
