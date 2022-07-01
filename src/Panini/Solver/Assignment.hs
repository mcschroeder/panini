module Panini.Solver.Assignment where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Panini.Syntax
import Prelude

-- | An assignment σ maps κ-variables to refinement predicates.
type Assignment = Map KVar Pred

class HasKVars a where
  -- | The set of κ-variables in a constraint or predicate.
  kvars :: a -> Set KVar

  -- | Replaces each κ-application κ(y̅) in a constraint or predicate with its
  -- solution σ(κ)[y̅/z̅] where z̅ = kparams(κ).
  apply :: Assignment -> a -> a

instance HasKVars Pred where
  kvars = \case
    PAppK k _ -> Set.singleton k
    PBin _ p1 p2 -> kvars p1 <> kvars p2
    PRel _ p1 p2 -> kvars p1 <> kvars p2
    PAnd ps      -> foldMap kvars ps
    PDisj p1 p2  -> kvars p1 <> kvars p2
    PImpl p1 p2  -> kvars p1 <> kvars p2
    PIff p1 p2   -> kvars p1 <> kvars p2
    PNot p       -> kvars p
    _            -> mempty

  apply s = \case
    PAppK k ys -> case Map.lookup k s of
      Just p  -> substN ys (kparams k) p
      Nothing -> PAppK k ys
    PBin o p1 p2 -> PBin o (apply s p1) (apply s p2)
    PRel r p1 p2 -> PRel r (apply s p1) (apply s p2)
    PAnd ps      -> PAnd (map (apply s) ps)
    PDisj p1 p2  -> PDisj (apply s p1) (apply s p2)
    PImpl p1 p2  -> PImpl (apply s p1) (apply s p2)
    PIff p1 p2   -> PIff (apply s p1) (apply s p2)
    PNot p       -> PNot (apply s p)
    PFun f ps    -> PFun f (map (apply s) ps)
    PExists x b p -> PExists x b (apply s p)
    PVar n       -> PVar n
    PCon c       -> PCon c

instance HasKVars Con where
  kvars = \case
    CHead p      -> kvars p
    CAnd c1 c2   -> kvars c1 <> kvars c2
    CAll _ _ p c -> kvars p <> kvars c

  apply s = \case
    CAll x b p c -> CAll x b (apply s p) (apply s c)
    CAnd c1 c2   -> CAnd (apply s c1) (apply s c2)
    CHead p      -> CHead (apply s p)

instance (Functor t, Foldable t, HasKVars a) => HasKVars (t a) where
  kvars = foldMap kvars
  apply s = fmap (apply s)
