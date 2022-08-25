module Panini.Syntax.Substitution where

import Data.List ((\\))
import Panini.Syntax.AST
import Panini.Syntax.Constraints
import Panini.Syntax.Names
import Panini.Syntax.Predicates
import Prelude

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
  subst :: Name -> Name -> a -> a
  
  -- | Returns all free variables of the given term.
  freeVars :: a -> [Name]

-- | @substN xs ys a@ substitutes each x for the corresponding y in a.
substN :: Subable a => [Name] -> [Name] -> a -> a
substN xs ys p = foldr (uncurry subst) p $ zip xs ys

------------------------------------------------------------------------------

instance Subable Type where
  subst x y = \case
    -- In a refined base type {n:b|r}, the value variable n names the
    -- value of type b that is being refined. Thus, we take n to be bound in r.    
    TBase n b r pv
      | y == n    -> TBase n b r pv  -- (1)
      | x == n    -> TBase ṅ b ṙ̲ pv  -- (2)
      | otherwise -> TBase n b r̲ pv  -- (3)
      where
        r̲ = subst x y r
        ṙ̲ = subst x y ṙ
        ṙ = subst ṅ n r
        ṅ = freshName n (y : freeVars r)

    -- In a dependent function type (n:t₁) → t₂, the name n binds t₁ in t₂. 
    -- Note that t₁ might itself contain (free) occurrences of n.
    TFun n t₁ t₂ pv
      | y == n    -> TFun n t̲₁̲ t₂ pv  -- (1)
      | x == n    -> TFun ṅ t̲₁̲ t̲₂̲̇ pv  -- (2)
      | otherwise -> TFun n t̲₁̲ t̲₂̲ pv  -- (3)
      where
        t̲₁̲ = subst x y t₁
        t̲₂̲ = subst x y t₂
        t̲₂̲̇ = subst x y t₂̇
        t₂̇ = subst ṅ n t₂
        ṅ = freshName n (y : freeVars t₂)

  freeVars = \case
    TBase v _ r _ -> freeVars r \\ [v]
    TFun x t₁ t₂ _ -> freeVars t₁ ++ (freeVars t₂ \\ [x])

instance Subable Reft where
  subst x y = \case
    Unknown -> Unknown
    Known p -> Known (subst x y p)
  freeVars = \case
    Unknown -> []
    Known p -> freeVars p

instance Subable Pred where  
  subst x y = \case
    PExists n b p
      | y == n    -> PExists n b p  -- (1)
      | x == n    -> PExists ṅ b ṗ̲  -- (2)
      | otherwise -> PExists n b p̲  -- (3)
      where
        p̲ = subst x y p
        ṗ̲ = subst x y ṗ
        ṗ = subst ṅ n p
        ṅ = freshName n (y : freeVars p)
        
    PRel r p₁ p₂ -> PRel r (subst x y p₁) (subst x y p₂)
    PImpl p₁ p₂  -> PImpl (subst x y p₁) (subst x y p₂)
    PIff p₁ p₂   -> PIff  (subst x y p₁) (subst x y p₂)
    PAnd ps      -> PAnd    (map (subst x y) ps)
    POr ps       -> POr     (map (subst x y) ps)
    PAppK k xs   -> PAppK k (map (subst x y) xs)
    PNot p₁      -> PNot (subst x y p₁)
    PTrue        -> PTrue
    PFalse       -> PFalse

  freeVars = \case
    PExists n _ p -> freeVars p \\ [n]
    PRel _ p₁ p₂  -> freeVars p₁ ++ freeVars p₂
    PImpl p₁ p₂   -> freeVars p₁ ++ freeVars p₂
    PIff  p₁ p₂   -> freeVars p₁ ++ freeVars p₂
    PAnd ps       -> concatMap freeVars ps
    POr ps        -> concatMap freeVars ps
    PAppK _ xs    -> xs
    PNot p₁       -> freeVars p₁
    PTrue         -> []
    PFalse        -> []

instance Subable PExpr where
  subst x y = \case
    PVar n
      | y == n    -> PVar x
      | otherwise -> PVar n

    PCon c       -> PCon c
    PBin o p₁ p₂ -> PBin o (subst x y p₁) (subst x y p₂)
    PFun f ps    -> PFun f (map (subst x y) ps)  -- TODO: what about f?
    PStrLen p    -> PStrLen (subst x y p)
    PStrAt p₁ p₂ -> PStrAt (subst x y p₁) (subst x y p₂)
    PStrSub p₁ p₂ p₃ -> PStrSub (subst x y p₁) (subst x y p₂) (subst x y p₃)
  
  freeVars = \case
    PVar n        -> [n]
    PBin _ p₁ p₂  -> freeVars p₁ ++ freeVars p₂
    PFun f ps     -> concatMap freeVars ps ++ [f]
    PCon _        -> []
    PStrLen p     -> freeVars p
    PStrAt p₁ p₂  -> freeVars p₁ ++ freeVars p₂
    PStrSub p₁ p₂ p₃ -> freeVars p₁ ++ freeVars p₂ ++ freeVars p₃

instance Subable Con where
  subst x y = \case
    CAll n b p c
      | y == n    -> CAll n b p c  -- (1)
      | x == n    -> CAll ṅ b ṗ̲ ċ̲  -- (2)
      | otherwise -> CAll n b p̲ c̲  -- (3)
      where        
        p̲ = subst x y p
        c̲ = subst x y c
        ṗ̲ = subst x y ṗ
        ċ̲ = subst x y ċ
        ṗ = subst ṅ n p
        ċ = subst ṅ n c
        ṅ = freshName n (y : freeVars p ++ freeVars c)

    CHead p    -> CHead (subst x y p)
    CAnd c₁ c₂ -> CAnd  (subst x y c₁) (subst x y c₂)

  freeVars = \case
    CHead p      -> freeVars p
    CAnd c₁ c₂   -> freeVars c₁ ++ freeVars c₂
    CAll n _ p c -> (freeVars p ++ freeVars c) \\ [n]

instance Subable Name where
  subst x y n = if y == n then x else n  
  freeVars n = [n]
