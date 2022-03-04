module Language.Panini.Checker where

import Data.List

import Language.Panini.Substitution
import Language.Panini.Syntax

-- | Type checker monad.
type TC a = Either TypeError a

failWith :: TypeError -> TC a
failWith = Left

data TypeError
  = InvalidSubtypingBase Base Base
  | InvalidSubtyping Type Type
  deriving (Show, Read)

------------------------------------------------------------------------------
-- Subtyping

{- | Subtyping rules.

@

  g |- forall v1:b. p1 => p2[v1/v2]
-------------------------------------  SUB-BASE
     g |- {v1:b|p1} <: {v2:b|p2}


  g |- s2 <: s1      g, x2:s2 |- t1[x2/x1] <: t2
--------------------------------------------------  SUB-FUN
          g |- x1:s1 -> t1 <: x2:s2 -> t2

@

-}
sub :: Type -> Type -> TC Con

-- [SUB-BASE]
sub (TBase v1 b1 (Known p1)) (TBase v2 b2 (Known p2))
  | b1 == b2  = return $ CAll v1 b1 p1 (CPred $ subst v1 v2 p2)
  | otherwise = failWith $ InvalidSubtypingBase b1 b2

-- [SUB-FUN]
sub (TFun x1 s1 t1) (TFun x2 s2 t2) = do
  cI <- sub s2 s1
  let t1' = subst x1 x2 t1
  cO <- implCon x2 s2 <$> sub t1' t2
  return $ CConj cI cO

sub t1 t2 = failWith $ InvalidSubtyping t1 t2

-- | Implication constraint @(x :: t) => c@.
implCon :: Name -> Type -> Con -> Con
implCon x t c = case t of
  TBase v b (Known p) -> CAll x b (subst x v p) c
  _                   -> c
