module Panini.Solver.Assignment where

import Data.Generics.Uniplate.Operations
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

class HasKVars a where
  -- | The set of κ-variables in a constraint or predicate.
  kvars :: a -> Set KVar

instance HasKVars Pred where
  kvars p = Set.fromList [k | PAppK k _ <- universe p]

instance HasKVars Con where
  kvars c = foldMap kvars $ universeBi @Con @Pred c

instance HasKVars FlatCon where
  kvars (FAll _ p q) = kvars p <> kvars q

instance HasKVars [FlatCon] where
  kvars = foldMap kvars

instance HasKVars Type where
  kvars = foldMap kvars . universeBi @Type @Pred

-------------------------------------------------------------------------------

-- | An assignment σ maps κ-variables to refinement predicates.
type Assignment = Map KVar Pred

class AssignKVars a where
  -- | Replaces each κ-application κ(y̅) in a constraint or predicate with its
  -- solution σ(κ)[y̅/z̅] where z̅ = kparams(κ).
  apply :: Assignment -> a -> a

instance AssignKVars Pred where
  apply s = transform $ \case
    PAppK k ys -> case Map.lookup k s of
      Just p  -> substN ys (kparams k) p
      Nothing -> PAppK k ys
    p -> p

instance AssignKVars Con where  
  apply s = transformBi @Con @Pred (apply s)

instance AssignKVars FlatCon where
  apply s (FAll xs p q) = FAll xs (apply s p) (apply s q)

instance AssignKVars [FlatCon] where
  apply s = fmap (apply s)

instance AssignKVars Type where
  apply s = transformBi @Type @Pred (apply s)
