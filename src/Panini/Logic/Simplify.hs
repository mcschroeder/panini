-- | This module contains functions to simplify predicates and constraints by
-- applying basic logical equivalences and dropping unnecessary quantifiers.
module Panini.Logic.Simplify (simplifyCon, simplifyPred) where

import Data.Generics.Uniplate.Operations
import Panini.Logic.Constraints
import Panini.Logic.Expressions
import Panini.Logic.Predicates
import Panini.Logic.Relations
import Panini.Names
import Panini.Primitives
import Panini.Substitution
import Prelude

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

      CAll _ _ PFalse _     -> CTrue
      CAll _ _ _      CTrue -> CTrue
      
      CAll x _ p c
        | x `notElem` (freeVars p <> freeVars c) -> case (p,c) of
          (PTrue , _      ) -> c
          (_     , CFalse ) -> CHead (PNot p)
          (_     , CHead q) -> CHead (PImpl p q)
          _                 -> CAll dummyName TUnit p c
        
        | CHead q <- c, p == q -> CTrue

        -- ∀x. x = y ⇒ φ(x)  ≡  φ(y)  where y is a primitive value
--        | Rel Eq (PVal (Var x')) (PVal y) <- p, x == x' -> go (subst y x c)
      
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
  PImpl _      PTrue  -> PTrue
  PImpl a      PFalse -> PNot a
  
  PImpl a b | a == b -> PTrue

  PIff PTrue  b      -> b
  PIff PFalse b      -> PNot b
  PIff a      PTrue  -> a
  PIff a      PFalse -> PNot a

  PIff a b | a == b -> PTrue

  PExists x _ p
    | x `notElem` freeVars p -> p
  
  PExists x _ (PRel (Rel Eq (PVar v1) (PVar v2)))  -- ∃x. x = y
    | x == v1 || x == v2 -> PTrue

  PRel (Rel Eq p q) | p == q -> PTrue
  PRel (Rel Le p q) | p == q -> PTrue
  PRel (Rel Ge p q) | p == q -> PTrue
  PRel (Rel Ne p q) | p == q -> PFalse
  PRel (Rel Lt p q) | p == q -> PFalse
  PRel (Rel Gt p q) | p == q -> PFalse

  PRel (Rel r (PVal (Con (I a _))) (PVal (Con (I b _)))) -> mkBoolPred $ evalRel r a b

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
