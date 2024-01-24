{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FunctionalDependencies #-}
module Panini.Syntax.Substitution where

import Data.Generics.Uniplate.Operations
import Data.Set (Set, (\\))
import Panini.Syntax.Expressions
import Panini.Syntax.Names
import Panini.Syntax.Predicates
import Panini.Syntax.Primitives
import Panini.Syntax.Relations
import Prelude

-- TODO: move instances into correpsonding modules?

------------------------------------------------------------------------------

-- | Types implementing capture-avoiding substitution.
--
-- In a substitution @p[x/y]@, @x@ is substituted for @y@ in @p@ (@x@ replaces
-- @y@). If @p@ is a binder, for example a lambda abstraction @λn.e@ that binds
-- the name @n@ inside @e@, then we need to deal with three possible scenarios:
--
-- (1) If the bound name @n@ is the same as the name @y@ that is being replaced,
--     then there can be no free occurrences of @y@ in @e@ and we can leave @e@
--     as-is.
--
-- (2) If the bound name @n@ is the same as the substitution @x@, then we need
--     to rename @n@ to something fresh that doesn't yet occur in @e@ (and also
--     isn't @y@) and update all occurrences of @n@ in @e@ accordingly. Don't
--     forget to continue substituting @x@ for @y@ in @e@!
--
-- (3) If the bound name @n@ is neither @x@ nor @y@, then we can just recurse
--     into @e@.
--
class Subable a v | a -> v where

  -- | Capture-avoiding substitution.
  --
  -- @subst x y a  ===  a[x/y]  ===  a where x replaces y@
  subst :: v -> Name -> a -> a
  
  -- | Returns all free variables of the given term.
  freeVars :: a -> Set Name

-- | @substN xs ys a@ substitutes each x for the corresponding y in a.
substN :: Subable a v => [v] -> [Name] -> a -> a
substN xs ys p = foldr (uncurry subst) p $ zip xs ys

------------------------------------------------------------------------------

instance Subable Pred Expr where  
  subst x y = \case
    PExists n b p
      | y == n      -> PExists n b            p   -- (1)
      | x == EVar n -> PExists ṅ b (subst x y ṗ)  -- (2)
      | otherwise   -> PExists n b (subst x y p)  -- (3)
      where
        ṗ = subst (EVar ṅ) n p
        ṅ = freshName n ([y] <> freeVars p)

    PAppK k xs -> PAppK k (map (subst x y) xs)
    PRel r     -> PRel (subst x y r)
    p          -> descend (subst x y) p

  freeVars = \case
    PTrue         -> []
    PFalse        -> []
    PAnd ps       -> mconcat $ map freeVars ps
    POr ps        -> mconcat $ map freeVars ps
    PImpl p1 p2   -> freeVars p1 <> freeVars p2
    PIff p1 p2    -> freeVars p1 <> freeVars p2
    PNot p        -> freeVars p
    PRel r        -> freeVars r
    PAppK _ xs    -> mconcat $ map freeVars xs
    PExists x _ p -> freeVars p \\ [x]
    
------------------------------------------------------------------------------

instance Subable Rel Expr where
  subst x y = descendBi (subst @Expr x y)  
  freeVars = mconcat . map (freeVars @Expr) . universeBi

------------------------------------------------------------------------------

instance Subable Expr Expr where
  subst x y = \case
    EVal (Var n) | y == n -> x
    e                     -> descend (subst x y) e

  freeVars = \case
    EVal (Var x)     -> [x]
    EVal (Con _)     -> []
    EAbs _           -> []
    EReg _           -> []
    ENot e           -> freeVars e
    EAdd e1 e2       -> freeVars e1 <> freeVars e2
    ESub e1 e2       -> freeVars e1 <> freeVars e2
    EMul e1 e2       -> freeVars e1 <> freeVars e2
    EStrLen e        -> freeVars e
    EStrAt e1 e2     -> freeVars e1 <> freeVars e2
    EStrSub e1 e2 e3 -> freeVars e1 <> freeVars e2 <> freeVars e3
    EFun _ es        -> mconcat (map freeVars es)

------------------------------------------------------------------------------

instance Subable Value Value where
  subst x y = \case
    Var n | y == n -> x
    v              -> v

  freeVars = \case
    Var n -> [n]
    Con _ -> []
