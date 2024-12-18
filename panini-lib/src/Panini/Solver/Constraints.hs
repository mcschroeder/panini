{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
module Panini.Solver.Constraints where

import Algebra.Lattice
import Data.Generics.Uniplate.Direct
import Data.Hashable
import Data.Set ((\\))
import Data.Set qualified as Set
import GHC.Generics
import Panini.Panic
import Panini.Pretty
import Panini.Syntax.Expressions
import Panini.Syntax.Names
import Panini.Syntax.Predicates
import Panini.Syntax.Primitives
import Panini.Syntax.Substitution
import Panini.Syntax.Relations
import Prelude

------------------------------------------------------------------------------

type Con = Con' Value

-- | Constraints are Horn clauses in negation normal form (NNF). They form a
-- tree, where each leaf is a goal ('CHead') and each node either quantifies
-- some variable to satisfy a hypothesis ('CAll') or conjoins two
-- sub-constraints ('CAnd').
data Con' a
  = CHead !(Pred' a)                  -- p
  | CAnd !(Con' a) !(Con' a)               -- c1 ∧ c2
  | CAll !Name !Base !(Pred' a) !(Con' a)  -- ∀(x:b). p ⟹ c  
  deriving stock 
    ( Eq
    , Ord  -- ^ structural ordering
    , Functor
    , Show, Read, Generic)

instance Hashable a => Hashable (Con' a)

pattern CTrue :: Con' a
pattern CTrue = CHead PTrue

pattern CFalse :: Con' a
pattern CFalse = CHead PFalse

-- | Similar to structural ordering, except 'CTrue' is the largest element.
instance Ord a => PartialOrder (Con' a) where
  _     ⊑ CTrue = True
  CTrue ⊑ _     = False
  a     ⊑ b     = a <= b

instance Ord a => MeetSemilattice (Con' a) where
  CTrue ∧ c2    = c2
  c1    ∧ CTrue = c1
  c1    ∧ c2    = CAnd c1 c2

instance Ord a => BoundedMeetSemilattice (Con' a) where
  top = CTrue

instance Uniplate (Con' a) where
  uniplate = \case
    CHead p      -> plate CHead |- p
    CAnd c1 c2   -> plate CAnd |* c1 |* c2
    CAll x b p c -> plate CAll |- x |- b |- p |* c

instance Biplate (Con' a) (Pred' a) where
  biplate = \case
    CHead p      -> plate CHead |* p
    CAnd c1 c2   -> plate CAnd |+ c1 |+ c2
    CAll x b p c -> plate CAll |- x |- b |* p |+ c

instance 
  (Uniplate (Expr' a), Biplate (Rel' a) (Expr' a)) 
  => Biplate (Con' a) (Expr' a) 
 where
  biplate = \case
    CHead p      -> plate CHead |+ p
    CAnd c1 c2   -> plate CAnd |+ c1 |+ c2
    CAll x b p c -> plate CAll |- x |- b |+ p |+ c

instance Pretty a => Pretty (Con' a) where
  pretty = \case
    CHead p -> pretty p
    CAnd c1 c2 -> align $ pretty c1 <+> wedge <\> pretty c2
    CAll x b p c -> parens $ case c of
      CHead _ ->          forall_ <+> pretty p <+> symImplies <+> pretty c
      _       -> hang 1 $ forall_ <+> pretty p <+> symImplies <\> pretty c
      where
        forall_ = symAll <> pretty x <> colon <> pretty b <> dot

instance (Uniplate (Expr' a), Subable (Expr' a) (Expr' a), Subable (Rel' a) (Expr' a)) => Subable (Con' a) (Expr' a) where
  subst x y = \case
    CAll n b p c
      | n == y       -> CAll n b            p             c   -- (1)
      | n `freeIn` x -> CAll ṅ b (subst x y ṗ) (subst x y ċ)  -- (2)
      | otherwise    -> CAll n b (subst x y p) (subst x y c)  -- (3)
      where
        ṗ = subst (EVar ṅ b) n p
        ċ = subst (EVar ṅ b) n c
        ṅ = freshName n ([y] <> freeVars p <> freeVars c <> freeVars x)

    CHead p    -> CHead (subst x y p)
    CAnd c₁ c₂ -> CAnd  (subst x y c₁) (subst x y c₂)

  freeVars = \case
    CHead p      -> freeVars p
    CAnd c₁ c₂   -> freeVars c₁ <> freeVars c₂
    CAll n _ p c -> (freeVars p <> freeVars c) \\ [n]

------------------------------------------------------------------------------

-- | A flat constraint of the form ∀(x₁:b₁,x₂:b₂,…). p ⇒ q where q is either a
-- single κ-variable application κ(y̅) or a concrete predicate free of
-- κ-variables.
data FlatCon = FAll [(Name,Base)] Pred Pred

-- | Flatten a constraint.
flat :: Con -> [FlatCon]
flat c₀ = [simpl [] [PTrue] c' | c' <- split c₀]
  where
    split (CAll x b p c) = [CAll x b p c' | c' <- split c]
    split (CHead p)      = [CHead p]
    split (CAnd c₁ c₂)   = split c₁ ++ split c₂

    simpl xs ps (CAll x b p c) = simpl ((x',b):xs) (p':ps) c'
      where
        x' = if x `elem` vs then freshName x vs else x
        vs = Set.fromList (map fst xs) <> mconcat (map freeVars ps)
        p' = if x' /= x then subst (EVar x' b) x p else p
        c' = if x' /= x then subst (EVar x' b) x c else c

    simpl xs ps (CHead q)      = FAll (reverse xs) (meets $ reverse ps) q
    simpl _  _  (CAnd _ _)     = impossible

-- | Whether or not a flat constraint has a κ application in its head.
horny :: FlatCon -> Bool
horny (FAll _ _ (PAppK _ _)) = True
horny _                      = False

instance Pretty FlatCon where
  pretty (FAll xs p q) = hang 2 $ sep 
    [ symAll <> binders xs <> dot
    , pretty p <+> symImplies <+> pretty q
    ]
   where
    binders = prettyTupleTight . map (\(x,b) -> pretty x <> colon <> pretty b)
