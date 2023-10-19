-- | This module contains functions to simplify predicates and constraints by
-- applying basic logical equivalences and dropping unnecessary quantifiers.
module Panini.Solver.Simplify (simplifyCon, simplifyPred) where

import Data.Generics.Uniplate.Operations
import Panini.Panic
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude
import Panini.Abstract.AExpr
import Panini.Abstract.AString (anyChar)
import Data.List qualified as List
import Data.Set qualified as Set
import Panini.Solver.Assignment

------------------------------------------------------------------------------

simplifyCon :: Con -> Con
simplifyCon = transform go . transformBi simplifyPred
  where
    go = \case
      CHead p -> CHead (simplifyPred p)
  
      CAnd CTrue  c2     -> c2
      CAnd c1     CTrue  -> c1
      CAnd CFalse _      -> CFalse
      CAnd _      CFalse -> CFalse      

      -- CAll _ _ PFalse _ -> CTrue
      
      -- c0@(CAll _ _ _ CTrue) | null (freeVars c0) -> CTrue
      -- c0@(CAll _ _ _ CTrue) -> CTrue
      
      CAll x _ p c
        | [y] <- Set.toList (freeVars p <> freeVars c), x == y, null (kvars p <> kvars c) -> CTrue
        | x `notElem` (freeVars p <> freeVars c) -> case (p,c) of
          (PTrue , _      ) -> c
          (_     , CFalse ) -> CHead (PNot p)
          (_     , CHead q) -> CHead (PImpl p q)
          _                 -> CAll dummyName TUnit p c
        
        | CHead q <- c, p == q -> CTrue

        -- ∀x. x = y ⇒ φ(x)  ≡  φ(y)  where y is a primitive value
--        | Rel Eq (EVal (Var x')) (EVal y) <- p, x == x' -> go (subst y x c)
      
      p -> p

simplifyPred :: Pred -> Pred
simplifyPred = transform $ \case  
  PAnd ps -> case simplifyAnds ps of
    [ ] -> PTrue
    [p] -> p
    ps' -> PAnd ps'

  POr ps -> case simplifyOrs ps of
    [ ] -> PFalse
    [p] -> p
    ps' -> POr ps'
  
  PImpl PTrue  b      -> b
  PImpl PFalse _      -> PTrue
  -- PImpl _      PTrue  -> PTrue
  PImpl a      PFalse -> PNot a
  
  PImpl a b | a == b -> PTrue

  PIff PTrue  b      -> b
  PIff PFalse b      -> PNot b
  PIff a      PTrue  -> a
  PIff a      PFalse -> PNot a

  PIff a b | a == b -> PTrue

  -- PExists x _ p
  --   | [y] <- Set.toList (freeVars p), x == y -> PTrue

  -- PExists x _ p
  --   | x `notElem` freeVars p -> p
  
  -- PExists x _ (PRel (Rel Eq (EVar v1) (EVar v2)))
  --   | x == v1 || x == v2 -> PTrue

  -- PExists _ _ _ -> PTrue

  -- PExists x _ p
  --   | isTrivial p -> PTrue
  --   | PAnd ps <- p
  --   , ([_],ps') <- List.partition isTrivial ps
  --   , x `notElem` freeVars (PAnd ps')
  --   -> PAnd ps'    
  --  where
  --   isTrivial = \case
  --     PRel (EVar v1 :=: EVar v2) -> v1 == x || v2 == x
  --     PIff (PRel (EVar v1 :=: EBool True _)) _ -> v1 == x
  --     _ -> False

  -- -- ∃x. x = z ∧ x = y₁ ∧ … ∧ x = yₙ   -->  y₁ = z ∧ … ∧ yₙ = z
  -- PExists x _ (PAnd (PRel (EVar x1 :=: z@(EVal _)):ps))
  --   | x == x1
  --   , ys <- extract ps
  --   , length ys == length ps 
  --   -> PAnd $ map (\y -> PRel (EVar y :=: z)) ys
  --   where
  --     extract (PRel (EVar a :=: EVar b) : rs)
  --       | x == a = b : extract rs
  --       | x == b = a : extract rs
  --     extract _ = []


  -- PExists x TInt (PAnd [PRel (EVar x1 :≥: EInt y pv), PRel (EVar x2 :=: EStrLen (EVar s))])
  --   | x == x1, x1 == x2
  --   -> PRel (EStrLen (EVar s) :≥: EInt y pv)

  -- -- ∃(x:𝔹). x = _ ⟺ q  -->  true
  -- PExists x TBool (PIff (PRel (EVar x1 :=: EBool _ _)) q) 
  --   | x == x1, x `notElem` freeVars q
  --   -> PTrue

  -- PExists x TString (PRel (EVar x1 :=: EStrAt (EVar s) i))
  --   | x == x1, x /= s
  --   -> PRel (EStrAt (EVar s) i :=: EStrA anyChar)

  PRel (Rel Eq p q) | p == q -> PTrue
  PRel (Rel Le p q) | p == q -> PTrue
  PRel (Rel Ge p q) | p == q -> PTrue
  PRel (Rel Ne p q) | p == q -> PFalse
  PRel (Rel Lt p q) | p == q -> PFalse
  PRel (Rel Gt p q) | p == q -> PFalse
  
  PRel (Rel r (EVal (Con (I a _))) (EVal (Con (I b _)))) -> case r of
    Eq -> mkBoolPred $ a == b
    Ne -> mkBoolPred $ a /= b
    _  -> impossible

  p -> p

simplifyAnds :: [Pred] -> [Pred]
simplifyAnds = go []
  where
    go qs (PTrue   : ps) = go qs ps
    go _  (PFalse  : _ ) = [PFalse]
    go qs (PAnd rs : ps) = go qs (rs ++ ps)
    go qs (p       : ps) = go (p:qs) ps
    go qs []             = reverse qs

simplifyOrs :: [Pred] -> [Pred]
simplifyOrs = go []
  where
    go qs (POr rs : ps) = go qs (rs ++ ps)
    go _  (PTrue  : _ ) = [PTrue]
    go qs (PFalse : ps) = go qs ps
    go qs (p      : ps) = go (p:qs) ps
    go qs []            = reverse qs
