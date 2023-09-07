module Panini.Abstract.Semantics where

import Algebra.Lattice
import Control.Monad
import Data.Generics.Uniplate.Operations qualified as Uniplate
import Data.Text qualified as Text
import Panini.Abstract.AChar as AChar
import Panini.Abstract.AInt as AInt
import Panini.Abstract.AString as AString
import Panini.Abstract.AValue
import Panini.Error
import Panini.Monad
import Panini.Panic
import Panini.Pretty
import Panini.Provenance
import Panini.Syntax
import Prelude

-- local notation -------------------------------------------------------------

(∈) :: Subable a v => Name -> a -> Bool
x ∈ e = x `elem` freeVars e

(∉) :: Subable a v => Name -> a -> Bool
x ∉ e = x `notElem` freeVars e

-------------------------------------------------------------------------------

abstractVar :: Name -> Base -> Rel -> Pan Expr
abstractVar x b r
  | x ∉ r                  = return $ topExpr b  
  | Just e <- abstract x r = return e
  | otherwise              = throwError $ AbstractionImpossible r x

concretizeVar :: Name -> Expr -> Pan Rel
concretizeVar x e = case e of
  EStrA s -> return $ EVar x :∈: EStrA s  
  _       -> throwError $ ConcretizationImpossible e x

topExpr :: Base -> Expr
topExpr TBool   = EAbs $ ABool top
topExpr TInt    = EAbs $ AInt top
topExpr TString = EAbs $ AString top
topExpr b       = panic $ "no" <+> symTop <+> "for " <+> pretty b

-------------------------------------------------------------------------------

abstract :: Name -> Rel -> Maybe Expr
abstract x r0 = case normRel r0 of
  r | x ∉ r -> Nothing

  {----------------------------------------------------------
            isolating x on the left-hand side
  ----------------------------------------------------------}

  -- ⟦ e₁ ▷ e₂ ⟧↑ₓ ≐ ⟦ e₂ ◁ e₁ ⟧↑ₓ  if x ∉ e₁
  r | x ∉ leftSide r -> abstract x =<< converse r

  -- ⟦ e₁ + e₂ ⋈ e₃ ⟧↑ₓ ≐ ⟦ e₁ ⋈ e₃ - e₂ ⟧↑ₓ  if x ∈ e₁ 
  Rel op (e1 :+: e2) e3 | x ∈ e1 -> abstract x $ Rel op e1 (e3 :-: e2)

  -- ⟦ e₁ + e₂ ⋈ e₃ ⟧↑ₓ ≐ ⟦ e₂ ⋈ e₃ - e₁ ⟧↑ₓ  if x ∈ e₂
  Rel op (e1 :+: e2) e3 | x ∈ e2 -> abstract x $ Rel op e2 (e3 :-: e1)
  
  -- ⟦ e₁ - e₂ ⋈ e₃ ⟧↑ₓ ≐ ⟦ e₁ ⋈ e₃ + e₂ ⟧↑ₓ  if x ∈ e₁
  Rel op (e1 :-: e2) e3 | x ∈ e1 -> abstract x $ Rel op e1 (e3 :+: e2)
    
  -- ⟦ e₁ - e₂ ⋈ e₃ ⟧↑ₓ ≐ ⟦ e₂ ⋈ e₁ + e₃ ⟧↑ₓ  if x ∈ e₂
  Rel op (e1 :-: e2) e3 | x ∈ e2 -> abstract x $ Rel op e2 (e1 :+: e3)


  {----------------------------------------------------------
            equalizing generic integer expressions
  ----------------------------------------------------------}

  -- ⟦ e₁ > e₂ ⟧↑ₓ ≐ ⟦ e₁ = e₂ + [1,+∞] ⟧↑ₓ
  e1 :>: e2 -> abstract x $ e1 :=: (e2 :+: EIntA (AInt.gt 0))

  -- ⟦ e₁ ≥ e₂ ⟧↑ₓ ≐ ⟦ e₁ = e₂ + [0,+∞] ⟧↑ₓ
  e1 :≥: e2 -> abstract x $ e1 :=: (e2 :+: EIntA (AInt.ge 0))

  -- ⟦ e₁ < e₂ ⟧↑ₓ ≐ ⟦ e₁ = e₂ + [-∞,-1] ⟧↑ₓ
  e1 :<: e2 -> abstract x $ e1 :=: (e2 :+: EIntA (AInt.lt 0))

  -- ⟦ e₁ ≤ e₂ ⟧↑ₓ ≐ ⟦ e₁ = e₂ + [-∞,0] ⟧↑ₓ
  e1 :≤: e2 -> abstract x $ e1 :=: (e2 :+: EIntA (AInt.le 0))


  {----------------------------------------------------------
              equalizing primitive inequalities
  ----------------------------------------------------------}

  -- ⟦ e ≠ b ⟧↑ₓ ≐ ⟦ e = b̅ ⟧↑ₓ
  e :≠: EBool b pv -> abstract x $ e :=: EBool (not b) pv

  -- ⟦ e ≠ b̂ ⟧↑ₓ ≐ ⟦ e = ¬b̂ ⟧↑ₓ
  e :≠: EBoolA b -> abstract x $ e :=: EBoolA (neg b)

  -- ⟦ e ≠ i ⟧↑ₓ ≐ ⟦ e = [-∞,i-1|i+1,∞] ⟧↑ₓ
  e :≠: EInt i _ -> abstract x $ e :=: EIntA (AInt.ne i)

  -- ⟦ e ≠ î ⟧↑ₓ ≐ ⟦ e = ¬î ⟧↑ₓ
  e :≠: EIntA i -> abstract x $ e :=: EIntA (neg i)

  -- ⟦ e ≠ c ⟧↑ₓ ≐ ⟦ e = Σ∖c  ⟧↑ₓ
  e :≠: EChar c _ -> abstract x $ e :=: EStrA (lit $ AChar.ne c)


  {---------------------------------------------------------
          abstracting simple variable assignments
  ---------------------------------------------------------}

  -- ⟦ x = e ⟧↑ₓ ≐ e  if x ∉ e
  EVar _x :=: e | x ∉ e -> Just e

  {----------------------------------------------------------
          abstracting string length expressions
  ----------------------------------------------------------}

  -- ⟦ |x| = i ⟧↑ₓ ≐ Σ^i
  EStrLen (EVar _x) :=: (EInt i _) 
    -> Just $ EStrA $ rep anyChar i
  
  -- ⟦ |x| = [i,+∞] ⟧↑ₓ ≐ Σ^(i)Σ*
  EStrLen (EVar _x) :=: EIntA a
    | Just (Fin i) <- AInt.minimum a
    , Just PosInf  <- AInt.maximum a
    , AInt.continuous a
    -> Just $ EStrA $ rep anyChar i <> star anyChar

  -- TODO: implement general case
  -- ⟦ |x| = [a₁,b₁|…|aₙ,bₙ] ⟧↑ₓ ≐ ???

  -- ⟦ |x[e₁..e₂]| = e ⟧↑ₓ ≐ ⟦ |x| = e + [0,∞] ⟧↑  if x ∉ e
  EStrLen (EStrSub (EVar x1) _e1 _e2) :=: e | x1 == x
    -> abstract x $ EStrLen (EVar x) :=: (e :+: EIntA (AInt.ge 0))
  
  {----------------------------------------------------------
          abstracting character-at-index expressions
  ----------------------------------------------------------}

  -- ⟦ x[i] = c ⟧↑ₓ ≐ Σ^(i)cΣ*
  EStrAt (EVar _x) (EInt i _) :=: EChar c _
    -> Just $ EStrA $ rep anyChar i <> lit (AChar.eq c) <> star anyChar

  -- ⟦ x[i] = ĉ ⟧↑ₓ ≐ Σ^(i)ĉΣ*
  EStrAt (EVar _x) (EInt i _) :=: EStrA c
    -> Just $ EStrA $ rep anyChar i <> c <> star anyChar

  -- TODO: this is an over-approximation! (or is it?)
  -- ⟦ s[x] = c ⟧↑ₓ ≐ |s| - [1,∞]
  EStrAt (EVar s) (EVar x1) :=: EChar _ _ | x1 == x
    -> Just $ EStrLen (EVar s) :-: (EIntA $ AInt.ge 1)
  
  {----------------------------------------------------------
              abstracting substring expressions
  ----------------------------------------------------------}

  -- note: the following abstractions are over-fitting on tests 013 and 016
  -- TODO: generalize these / are they even correct?
  
  -- ⟦ s[0..x] = t ⟧↑ₓ ≐ |t| - 1
  EStrSub (EVar _s) (EInt 0 _) (EVar x1) :=: EVar t | x1 == x 
    -> Just $ EStrLen (EVar t) :-: EInt 1 NoPV

  -- ⟦ s[0..|x|] = x ⟧↑ₓ ≐ s[0..|x|]
  EStrSub (EVar s) (EInt 0 pv) (EStrLen (EVar x1)) :=: EVar x2 | x1 == x, x2 == x 
    -> Just $ EStrSub (EVar s) (EInt 0 pv) (EStrLen (EVar x))

  -- ⟦ x[0..|s|] = t ⟧↑ₓ ≐ tΣ*
  EStrSub (EVar x1) (EInt 0 _) (EStrLen (EVar _s)) :=: EStr t _ | x1 == x 
    -> Just $ EStrA $ AString.eq (Text.unpack t) <> star anyChar
    -- TODO: what if |s| /= |t| ??

  
  {----------------------------------------------------------
     whereof one cannot speak, thereof one must be silent 
  ----------------------------------------------------------}
  
  _ -> Nothing

-------------------------------------------------------------------------------

-- | Independently normalize each side of a relation.
normRel :: Rel -> Rel
normRel (Rel op e1 e2) = Rel op (norm e1) (norm e2)

-- | Normalize an expression by partial evaluation.
norm :: Expr -> Expr
norm = Uniplate.rewrite $ \case
  ENot (EBool  a pv) -> Just $ EBool  (not a) pv
  ENot (EBoolA a)    -> Just $ EBoolA (neg a)
    
  EInt  0 _ :+: e         -> Just e
  e         :+: EInt  0 _ -> Just e  
  EInt  a _ :+: EInt  b _ -> Just $ EInt (a + b) NoPV  
  EIntA a   :+: EInt  b _ -> Just $ EAbs $ AInt $ AInt.add a (AInt.eq b)
  EInt  a _ :+: EIntA b   -> Just $ EAbs $ AInt $ AInt.add (AInt.eq a) b
  EIntA a   :+: EIntA b   -> Just $ EAbs $ AInt $ AInt.add a b

  e         :-: EInt  0 _ -> Just e
  EInt  a _ :-: EInt  b _ -> Just $ EInt (a - b) NoPV
  EIntA a   :-: EInt  b _ -> Just $ EAbs $ AInt $ AInt.sub a (AInt.eq b)
  EInt  a _ :-: EIntA b   -> Just $ EAbs $ AInt $ AInt.sub (AInt.eq a) b
  EIntA a   :-: EIntA b   -> Just $ EAbs $ AInt $ AInt.sub a b

  -- if nothing else works, maybe re-associating will help
  (e1 :+: e2) :+: e3 -> Just $ e1 :+: (e2 :+: e3)

  _ -> Nothing
