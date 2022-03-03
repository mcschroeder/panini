module Language.Panini.Checker where

import Language.Panini.Syntax

-- | Type checker monad.
type TC a = Either TypeError a

failWith :: TypeError -> TC a
failWith = Left

data TypeError
  = InvalidSubtypingBase BaseType BaseType
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
sub (Base v1 b1 (Known p1)) (Base v2 b2 (Known p2))
  | b1 == b2  = return $ CAll v1 b1 p1 (CPred $ subst p2 v1 v2)
  | otherwise = failWith $ InvalidSubtypingBase b1 b2

-- [SUB-FUN]
sub (Pi x1 s1 t1) (Pi x2 s2 t2) = do
  cI <- sub s2 s1
  let t1' = subst t1 x1 x2
  cO <- implCon x2 s2 <$> sub t1' t2
  return $ CConj cI cO

sub t1 t2 = failWith $ InvalidSubtyping t1 t2

-- | Implication constraint @(x :: t) => c@.
implCon :: Name -> Type -> Con -> Con
implCon x t c = case t of
  Base v b (Known p) -> CAll x b (subst p x v) c
  _                  -> c


------------------------------------------------------------------------------
-- Substitution

-- TODO

class Subable a where
  -- | Substitution @t[x/y]@ where @x@ replaces @y@ in the @t@.
  subst :: a -> Name -> Name -> a

instance Subable Name where
  subst n x y = if n == y then x else n

instance Subable Pred where
  subst p x y = case p of
    PTrue -> PTrue
    PFalse -> PFalse    
    PVar n -> PVar (subst n x y)
    PBin o p1 p2 -> PBin o (subst p1 x y) (subst p2 x y)
    PRel r p1 p2 -> PRel r (subst p1 x y) (subst p2 x y)
    PConj p1 p2 -> PConj (subst p1 x y) (subst p2 x y)
    PDisj p1 p2 -> PDisj (subst p1 x y) (subst p2 x y)
    PImpl p1 p2 -> PImpl (subst p1 x y) (subst p2 x y)
    PIff p1 p2 -> PIff (subst p1 x y) (subst p2 x y)
    PNot p1 -> PNot (subst p1 x y)
    PFun f ps -> PFun f (map (\p1 -> subst p1 x y) ps)

instance Subable Type where
  subst t x y = case t of
    Base n b r -> Base (subst n x y) b (subst r x y)
    Pi n t1 t2 -> Pi (subst n x y) (subst t1 x y) (subst t2 x y)

instance Subable Refinement where
  subst Unknown   _ _ = Unknown
  subst (Known p) x y = Known (subst p x y)
