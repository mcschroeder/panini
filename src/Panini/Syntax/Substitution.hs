{-# LANGUAGE OverloadedLists #-}
module Panini.Syntax.Substitution where

import Data.Generics.Uniplate.Operations
import Data.Set (Set, (\\))
import Panini.Syntax.AST
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
class Subable a where

  -- | Capture-avoiding substitution.
  --
  -- @subst x y a  ===  a[x/y]  ===  a where x replaces y@
  subst :: Value -> Name -> a -> a
  
  -- | Returns all free variables of the given term.
  freeVars :: a -> Set Name

-- | @substN xs ys a@ substitutes each x for the corresponding y in a.
substN :: Subable a => [Value] -> [Name] -> a -> a
substN xs ys p = foldr (uncurry subst) p $ zip xs ys

------------------------------------------------------------------------------

instance Subable Type where
  subst x y = \case
    -- In a refined base type {n:b|r}, the value variable n names the
    -- value of type b that is being refined. Thus, we take n to be bound in r.    
    TBase n b r pv
      | y == n     -> TBase n b            r  pv  -- (1)
      | x == Var n -> TBase ṅ b (subst x y ṙ) pv  -- (2)
      | otherwise  -> TBase n b (subst x y r) pv  -- (3)
      where
        ṙ = subst (Var ṅ) n r
        ṅ = freshName n ([y] <> freeVars r)

    -- In a dependent function type (n:t₁) → t₂, the name n binds t₁ in t₂. 
    -- Note that t₁ might itself contain (free) occurrences of n.
    TFun n t₁ t₂ pv
      | y == n     -> TFun n (subst x y t₁)            t₂  pv  -- (1)
      | x == Var n -> TFun ṅ (subst x y t₁) (subst x y t₂̇) pv  -- (2)
      | otherwise  -> TFun n (subst x y t₁) (subst x y t₂) pv  -- (3)
      where
        t₂̇ = subst (Var ṅ) n t₂
        ṅ = freshName n ([y] <> freeVars t₂)

  freeVars = \case
    TBase v _ r _ -> freeVars r \\ [v]
    TFun x t₁ t₂ _ -> (freeVars t₁ <> freeVars t₂) \\ [x]

------------------------------------------------------------------------------

instance Subable Reft where
  subst x y = descendBi (subst @Pred x y)  
  freeVars = mconcat . map (freeVars @Pred) . universeBi

------------------------------------------------------------------------------

instance Subable Pred where  
  subst x y = \case
    PExists n b p
      | y == n     -> PExists n b            p   -- (1)
      | x == Var n -> PExists ṅ b (subst x y ṗ)  -- (2)
      | otherwise  -> PExists n b (subst x y p)  -- (3)
      where
        ṗ = subst (Var ṅ) n p
        ṅ = freshName n ([y] <> freeVars p)

    PAppK k xs -> PAppK k (map (subst x y) xs)
    PRel r     -> PRel (subst x y r)
    p          -> descend (subst x y) p

  freeVars p0 = mconcat . flip map (universe p0) $ \case
    PExists n _ p -> freeVars p \\ [n]    
    PAppK _ xs    -> mconcat $ map freeVars xs
    PRel r        -> freeVars r    
    _             -> []
    
------------------------------------------------------------------------------

instance Subable Rel where
  subst x y = descendBi (subst @Expr x y)  
  freeVars = mconcat . map (freeVars @Expr) . universeBi

------------------------------------------------------------------------------

instance Subable Expr where
  subst x y = descendBi (subst @Value x y)    
  
  freeVars e0 = mconcat . flip map (universe e0) $ \case
    EVal (Var n) -> [n]
    EFun f es    -> [f] <> mconcat (map freeVars es)
    _            -> []

------------------------------------------------------------------------------

instance Subable Value where
  subst x y = \case
    Var n | y == n -> x
    v              -> v

  freeVars = \case
    Var n -> [n]
    Con _ -> []
