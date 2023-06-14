module Panini.Logic.Solver.Assignment where

import Data.Generics.Uniplate.Operations
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Panini.Logic.Constraints
import Panini.Logic.Predicates
import Panini.Substitution
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
  kvars p = Set.fromList [k | PAppK k _ <- universe p]
  apply s = transform $ \case
    PAppK k ys -> case Map.lookup k s of
      Just p  -> substN ys (kparams k) p
      Nothing -> PAppK k ys
    p -> p

instance HasKVars Con where
  kvars c = foldMap kvars $ universeBi @Con @Pred  c
  apply s = transformBi @Con @Pred (apply s)

instance HasKVars FlatCon where
  kvars (FAll _ p q) = kvars p <> kvars q
  apply s (FAll xs p q) = FAll xs (apply s p) (apply s q)

instance (Functor t, Foldable t, HasKVars a) => HasKVars (t a) where
  kvars = foldMap kvars
  apply s = fmap (apply s)
