{-# LANGUAGE FunctionalDependencies #-}
module Panini.Syntax.Substitution where

import Data.Set (Set)
import Panini.Syntax.Names
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
-- (2) If the bound name @n@ occurs free in the substitution @x@, then we need
--     to rename @n@ to something fresh that doesn't yet occur in @x@ or @e@
--     (and also isn't @y@) and update all occurrences of @n@ in @e@
--     accordingly. Don't forget to continue substituting @x@ for @y@ in @e@!
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


-- | Whether a particular name occurs free in a given term.
freeIn :: Subable a v => Name -> a -> Bool
freeIn x a = x `elem` freeVars a

-- | Whether a particular name does not occur free in a given term.
notFreeIn :: Subable a v => Name -> a -> Bool
notFreeIn x = not . freeIn x
