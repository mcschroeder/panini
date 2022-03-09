module Language.Panini.Substitution where

import Data.List

import Language.Panini.Syntax

------------------------------------------------------------------------------

-- | Types implementing capture-avoiding substitution.
--
-- When substituting @x@ for @y@ in @p@, and @p@ is a binder, for example a
-- lambda abstraction @λn.e@ that binds the name @n@ inside @e@, then we need to
-- deal with three possible scenarios:
--
-- 1. If the bound name @n@ is the same as the name @y@ that we are replacing,
--    then there can be no free occurrences of @y@ in @e@ and we can leave @p@
--    as-is.
--
-- 2. If the substitution @x@ is a variable and the bound name @n@ is the same
--    as @x@, then we need to rename @n@ to something fresh that doesn't yet
--    occur in @e@ (and also isn't @y@) and update all occurrences of @n@ in @e@
--    accordingly. Don't forget to continue substituting @x@ for @y@ in @e@!
--
-- 3. If the bound name @n@ is neither @x@ nor @y@, then we can just recurse
--    into @e@.
--
class Subable a where

  -- | Capture-avoiding substitution.
  --
  -- @subst x y a  ===  a[x/y]  ===  a where x replaces y@
  subst :: Value -> Name -> a -> a
  
  -- | Returns all free variables of the given term.
  freeVars :: a -> [Name]

------------------------------------------------------------------------------

instance Subable Type where
  subst x y = \case
    -- In a refined base type {n:b|r}, the value variable n names the
    -- value of type b that is being refined. Thus, we take n to be bound in r.    
    TBase n b r
      | y == n -> TBase n b r                              -- (1)
      | x == V n -> let n' = freshName n (y : freeVars r)  -- (2)
                        r' = subst (V n') n r
                    in TBase n' b (subst x y r')
      | otherwise -> TBase n b (subst x y r)               -- (3)

    -- In a dependent function type n:t1 -> t2, the name n binds t1 in t2. 
    -- Note that t1 might itself contain (free) occurrences of n.
    TFun n t1 t2
      | y == n -> TFun n (subst x y t1) t2                   -- (1)
      | x == V n -> let n'  = freshName n (y : freeVars t2)  -- (2)
                        t2' = subst (V n') n t2
                    in TFun n' (subst x y t1) (subst x y t2')
      | otherwise -> TFun n (subst x y t1) (subst x y t2)    -- (3)

  freeVars = \case
    TBase v b r -> freeVars r \\ [v]
    TFun x t1 t2 -> freeVars t1 ++ (freeVars t2 \\ [x])

instance Subable Reft where
  subst x y = \case
    Unknown -> Unknown
    Known p -> Known (subst x y p)
  freeVars = \case
    Unknown -> []
    Known p -> freeVars p

-- | Note that the predicate language does not have binders, so we don't need to
-- worry about name capture in here.
instance Subable Pred where  
  subst x y = \case
    PVal v -> PVal (subst x y v)
    PBin o p1 p2 -> PBin o (subst x y p1) (subst x y p2)
    PRel r p1 p2 -> PRel r (subst x y p1) (subst x y p2)
    PConj p1 p2 -> PConj (subst x y p1) (subst x y p2)
    PDisj p1 p2 -> PDisj (subst x y p1) (subst x y p2)
    PImpl p1 p2 -> PImpl (subst x y p1) (subst x y p2)
    PIff p1 p2 -> PIff (subst x y p1) (subst x y p2)
    PNot p1 -> PNot (subst x y p1)
    PFun f ps -> PFun f (map (subst x y) ps)
  freeVars = \case
    PVal (V n) -> [n]
    PVal _ -> []
    PBin o p1 p2 -> freeVars p1 ++ freeVars p2
    PRel r p1 p2 -> freeVars p1 ++ freeVars p2
    PConj p1 p2 -> freeVars p1 ++ freeVars p2
    PDisj p1 p2 -> freeVars p1 ++ freeVars p2
    PImpl p1 p2 -> freeVars p1 ++ freeVars p2
    PIff p1 p2 -> freeVars p1 ++ freeVars p2
    PNot p1 -> freeVars p1
    PFun f ps -> concatMap freeVars ps

instance Subable Con where
  subst x y = \case
    CPred p -> CPred (subst x y p)
    CConj c1 c2 -> CConj (subst x y c1) (subst x y c2)
    CAll n b p c
      | y == n -> CAll n b p c                                           -- (1)
      | x == V n -> let n' = freshName n (y : freeVars p ++ freeVars c)  -- (2)
                        p' = subst (V n') n p
                        c' = subst (V n') n c
                    in CAll n' b p' c'
      | otherwise -> CAll n b (subst x y p) (subst x y c)                -- (3)
  
  freeVars = \case
    CPred p -> freeVars p
    CConj c1 c2 -> freeVars c1 ++ freeVars c2
    CAll n _ p c -> (freeVars p ++ freeVars c) \\ [n]

instance Subable Value where
  subst x y (V n) | y == n = x
  subst _ _ v = v
  freeVars (V n) = [n]
  freeVars _ = []
