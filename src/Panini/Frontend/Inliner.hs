module Panini.Frontend.Inliner where

import Data.Generics.Uniplate.Operations
import Panini.Syntax
import Prelude

inlineProgram :: Program -> Program
inlineProgram = map $ \case
  Define x t -> Define x (inline t)
  s          -> s

inline :: Term -> Term
inline = rewrite $ \case
  -- inline atomic values
  Let x (Val v) e2 _ -> Just $ subst v x e2

  -- inline nullary rec bindings that occur as an artifact of transpilation
  Rec x (TBase _ TUnit _ _) e1 e2 _ 
    | let e2' = inlineTailCall x e1 e2, e2' /= e2 -> Just e2'
  
  -- reorder let bindings whose RHS is immediately another let-binding
  Let x (Let y ey1 ey2 pvy) ex2 pvx 
    -> Just $ Let y ey1 (Let x ey2 ex2 pvx) pvy

  _ -> Nothing

-- | @inlineTailCall x f e@ inlines the term @f@ into @e@ at all occurences of
-- the variable @x@ in tail-call position.
inlineTailCall :: Name -> Term -> Term -> Term
inlineTailCall x f = go
 where
  go = \case
    Val (Var y)    | x == y                          -> f
    Let y e1 e2 pv | x /= y, x `notElem` freeVars e1 -> Let y e1 (go e2) pv
    If v e1 e2 pv  | x `notElem` freeVars v          -> If v (go e1) (go e2) pv
    e0                                               -> e0

