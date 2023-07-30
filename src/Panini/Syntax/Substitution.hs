module Panini.Syntax.Substitution where

import Data.List ((\\))
import Panini.Logic.Constraints
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
  freeVars :: a -> [Name]

-- | @substN xs ys a@ substitutes each x for the corresponding y in a.
substN :: Subable a => [Value] -> [Name] -> a -> a
substN xs ys p = foldr (uncurry subst) p $ zip xs ys

------------------------------------------------------------------------------

instance Subable Type where
  subst x y = \case
    -- In a refined base type {n:b|r}, the value variable n names the
    -- value of type b that is being refined. Thus, we take n to be bound in r.    
    TBase n b r pv
      | y == n     -> TBase n b r pv  -- (1)
      | x == Var n -> TBase ṅ b ṙ̲ pv  -- (2)
      | otherwise  -> TBase n b r̲ pv  -- (3)
      where
        r̲ = subst x y r
        ṙ̲ = subst x y ṙ
        ṙ = subst (Var ṅ) n r
        ṅ = freshName n (y : freeVars r)

    -- In a dependent function type (n:t₁) → t₂, the name n binds t₁ in t₂. 
    -- Note that t₁ might itself contain (free) occurrences of n.
    TFun n t₁ t₂ pv
      | y == n     -> TFun n t̲₁̲ t₂ pv  -- (1)
      | x == Var n -> TFun ṅ t̲₁̲ t̲₂̲̇ pv  -- (2)
      | otherwise  -> TFun n t̲₁̲ t̲₂̲ pv  -- (3)
      where
        t̲₁̲ = subst x y t₁
        t̲₂̲ = subst x y t₂
        t̲₂̲̇ = subst x y t₂̇
        t₂̇ = subst (Var ṅ) n t₂
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
      | y == n     -> PExists n b p  -- (1)
      | x == Var n -> PExists ṅ b ṗ̲  -- (2)
      | otherwise  -> PExists n b p̲  -- (3)
      where
        p̲ = subst x y p
        ṗ̲ = subst x y ṗ
        ṗ = subst (Var ṅ) n p
        ṅ = freshName n (y : freeVars p)
        
    PImpl p₁ p₂  -> PImpl (subst x y p₁) (subst x y p₂)
    PIff p₁ p₂   -> PIff  (subst x y p₁) (subst x y p₂)
    PAnd ps      -> PAnd    (map (subst x y) ps)
    POr ps       -> POr     (map (subst x y) ps)
    PAppK k xs   -> PAppK k (map (subst x y) xs)
    PNot p₁      -> PNot (subst x y p₁)
    PRel p      -> PRel (subst x y p)
    PTrue        -> PTrue
    PFalse       -> PFalse    

  freeVars = \case
    PExists n _ p -> freeVars p \\ [n]    
    PImpl p₁ p₂   -> freeVars p₁ ++ freeVars p₂
    PIff  p₁ p₂   -> freeVars p₁ ++ freeVars p₂
    PAnd ps       -> concatMap freeVars ps
    POr ps        -> concatMap freeVars ps
    PAppK _ xs    -> concatMap freeVars xs
    PNot p₁       -> freeVars p₁
    PRel p       -> freeVars p
    PTrue         -> []
    PFalse        -> []
    

instance Subable Rel where
  subst x y = \case
    Rel r p₁ p₂ -> Rel r (subst x y p₁) (subst x y p₂)
  
  freeVars = \case
    Rel _ p₁ p₂  -> freeVars p₁ ++ freeVars p₂

instance Subable Expr where
  subst x y = \case
    EVal (Var n)
      | y == n    -> EVal x
      | otherwise -> EVar n

    EVal (Con c) -> EVal (Con c)
    EAdd p₁ p₂   -> EAdd (subst x y p₁) (subst x y p₂)
    ESub p₁ p₂   -> ESub (subst x y p₁) (subst x y p₂)
    EMul p₁ p₂   -> EMul (subst x y p₁) (subst x y p₂)
    EStrLen p    -> EStrLen (subst x y p)
    EStrAt p₁ p₂ -> EStrAt (subst x y p₁) (subst x y p₂)
    EStrSub p₁ p₂ p₃ -> EStrSub (subst x y p₁) (subst x y p₂) (subst x y p₃)
    EFun f ps -> EFun f (map (subst x y) ps)

    --TODO
    ENot p -> ENot (subst x y p)
    EAbs a -> EAbs a
  
  freeVars = \case
    EVal (Var n)  -> [n]
    EVal (Con _)  -> []
    EFun f ps     -> f : concatMap freeVars ps
    EAdd p₁ p₂    -> freeVars p₁ ++ freeVars p₂
    ESub p₁ p₂    -> freeVars p₁ ++ freeVars p₂
    EMul p₁ p₂    -> freeVars p₁ ++ freeVars p₂
    EStrLen p     -> freeVars p
    EStrAt p₁ p₂  -> freeVars p₁ ++ freeVars p₂
    EStrSub p₁ p₂ p₃ -> freeVars p₁ ++ freeVars p₂ ++ freeVars p₃

    --TODO
    ENot p -> freeVars p
    EAbs _ -> []

instance Subable Con where
  subst x y = \case
    CAll n b p c
      | y == n     -> CAll n b p c  -- (1)
      | x == Var n -> CAll ṅ b ṗ̲ ċ̲  -- (2)
      | otherwise  -> CAll n b p̲ c̲  -- (3)
      where        
        p̲ = subst x y p
        c̲ = subst x y c
        ṗ̲ = subst x y ṗ
        ċ̲ = subst x y ċ
        ṗ = subst (Var ṅ) n p
        ċ = subst (Var ṅ) n c
        ṅ = freshName n (y : freeVars p ++ freeVars c)

    CHead p    -> CHead (subst x y p)
    CAnd c₁ c₂ -> CAnd  (subst x y c₁) (subst x y c₂)

  freeVars = \case
    CHead p      -> freeVars p
    CAnd c₁ c₂   -> freeVars c₁ ++ freeVars c₂
    CAll n _ p c -> (freeVars p ++ freeVars c) \\ [n]

instance Subable Value where
  subst x y = \case
    Var n | y == n -> x
    v -> v
  freeVars = \case
    Var n -> [n]
    Con _ -> []
