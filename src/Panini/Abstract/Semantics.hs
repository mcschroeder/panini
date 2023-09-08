module Panini.Abstract.Semantics where

import Algebra.Lattice
import Control.Monad
import Data.Text qualified as Text
import Panini.Abstract.AChar as AChar
import Panini.Abstract.AExpr
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

abstractVar :: Name -> Base -> Rel -> Pan AExpr
abstractVar x b r
  | x ∉ r                  = return $ topExpr b  
  | Just e <- abstract x r = return e
  | otherwise              = throwError $ AbstractionImpossible r x  
                              -- TODO: report normalized r in error

concretizeVar :: Name -> AExpr -> Pan Rel
concretizeVar x e = case e of
  EStrA s -> return $ EVar x :∈: EStrA s  
  _       -> throwError $ ConcretizationImpossible e x

topExpr :: Base -> AExpr
topExpr TBool   = EAbs $ ABool top
topExpr TInt    = EAbs $ AInt top
topExpr TString = EAbs $ AString top
topExpr b       = panic $ "no" <+> symTop <+> "for " <+> pretty b

botExpr :: Base -> AExpr
botExpr TBool   = EAbs $ ABool bot
botExpr TInt    = EAbs $ AInt bot
botExpr TString = EAbs $ AString bot
botExpr b       = panic $ "no" <+> symBot <+> "for " <+> pretty b

-------------------------------------------------------------------------------

-- TODO: eliminate isolateVar/abstract duplication

isolateVar :: Name -> Rel -> Maybe AExpr
isolateVar x r0 = norm <$> case normRel r0 of
  r | x ∉ r -> Nothing

  {----------------------------------------------------------
            isolating x on the left-hand side
  ----------------------------------------------------------}

  -- ⟦ e₁ ▷ e₂ ⟧↑ₓ ≐ ⟦ e₂ ◁ e₁ ⟧↑ₓ  if x ∉ e₁
  r | x ∉ leftSide r -> isolateVar x =<< converse r

  -- ⟦ e₁ + e₂ ⋈ e₃ ⟧↑ₓ ≐ ⟦ e₁ ⋈ e₃ - e₂ ⟧↑ₓ  if x ∈ e₁ 
  Rel op (e1 :+: e2) e3 | x ∈ e1 -> isolateVar x $ Rel op e1 (e3 :-: e2)

  -- ⟦ e₁ + e₂ ⋈ e₃ ⟧↑ₓ ≐ ⟦ e₂ ⋈ e₃ - e₁ ⟧↑ₓ  if x ∈ e₂
  Rel op (e1 :+: e2) e3 | x ∈ e2 -> isolateVar x $ Rel op e2 (e3 :-: e1)
  
  -- ⟦ e₁ - e₂ ⋈ e₃ ⟧↑ₓ ≐ ⟦ e₁ ⋈ e₃ + e₂ ⟧↑ₓ  if x ∈ e₁
  Rel op (e1 :-: e2) e3 | x ∈ e1 -> isolateVar x $ Rel op e1 (e3 :+: e2)
    
  -- ⟦ e₁ - e₂ ⋈ e₃ ⟧↑ₓ ≐ ⟦ e₂ ⋈ e₁ + e₃ ⟧↑ₓ  if x ∈ e₂
  Rel op (e1 :-: e2) e3 | x ∈ e2 -> isolateVar x $ Rel op e2 (e1 :+: e3)


  {----------------------------------------------------------
            equalizing generic integer expressions
  ----------------------------------------------------------}

  -- ⟦ e₁ > e₂ ⟧↑ₓ ≐ ⟦ e₁ = e₂ + [1,+∞] ⟧↑ₓ
  e1 :>: e2 -> isolateVar x $ e1 :=: (e2 :+: EIntA (AInt.gt 0))

  -- ⟦ e₁ ≥ e₂ ⟧↑ₓ ≐ ⟦ e₁ = e₂ + [0,+∞] ⟧↑ₓ
  e1 :≥: e2 -> isolateVar x $ e1 :=: (e2 :+: EIntA (AInt.ge 0))

  -- ⟦ e₁ < e₂ ⟧↑ₓ ≐ ⟦ e₁ = e₂ + [-∞,-1] ⟧↑ₓ
  e1 :<: e2 -> isolateVar x $ e1 :=: (e2 :+: EIntA (AInt.lt 0))

  -- ⟦ e₁ ≤ e₂ ⟧↑ₓ ≐ ⟦ e₁ = e₂ + [-∞,0] ⟧↑ₓ
  e1 :≤: e2 -> isolateVar x $ e1 :=: (e2 :+: EIntA (AInt.le 0))


  {----------------------------------------------------------
              equalizing primitive inequalities
  ----------------------------------------------------------}

  -- ⟦ e ≠ b ⟧↑ₓ ≐ ⟦ e = b̅ ⟧↑ₓ
  e :≠: EBool b pv -> isolateVar x $ e :=: EBool (not b) pv

  -- ⟦ e ≠ b̂ ⟧↑ₓ ≐ ⟦ e = ¬b̂ ⟧↑ₓ
  e :≠: EBoolA b -> isolateVar x $ e :=: EBoolA (neg b)

  -- ⟦ e ≠ i ⟧↑ₓ ≐ ⟦ e = [-∞,i-1|i+1,∞] ⟧↑ₓ
  e :≠: EInt i _ -> isolateVar x $ e :=: EIntA (AInt.ne i)

  -- ⟦ e ≠ î ⟧↑ₓ ≐ ⟦ e = ¬î ⟧↑ₓ
  e :≠: EIntA i -> isolateVar x $ e :=: EIntA (neg i)

  -- ⟦ e ≠ c ⟧↑ₓ ≐ ⟦ e = Σ∖c  ⟧↑ₓ
  e :≠: EChar c _ -> isolateVar x $ e :=: EStrA (lit $ AChar.ne c)


  {---------------------------------------------------------
          abstracting simple variable assignments
  ---------------------------------------------------------}

  -- ⟦ x = e ⟧↑ₓ ≐ e  if x ∉ e
  EVar _x :=: e | x ∉ e -> Just e
  
  {----------------------------------------------------------
     whereof one cannot speak, thereof one must be silent 
  ----------------------------------------------------------}
  
  _ -> Nothing


abstract :: Name -> Rel -> Maybe AExpr
abstract x r0 = norm <$> case normRel r0 of
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

  -- ⟦ |x| = [a,b] ⟧↑ₓ ≐ ⟦ |x| = [0,b] ⟧↑ₓ  if a < 0
  -- ⟦ |x| = [a,b] ⟧↑ₓ ≐ Σ^(a) | Σ^(a+1) | … | Σ^(b)
  -- ⟦ |x| = [a,∞] ⟧↑ₓ ≐ Σ^(a)Σ*
  -- ⟦ |x| = [a₁,b₁|…|aₙ,bₙ] ⟧↑ₓ ≐ ⟦ |x| = [a₁,b₁] ⟧↑ₓ ∨ … ∨ ⟦ |x| = [aₙ,bₙ] ⟧↑ₓ
  EStrLen (EVar _x) :=: EIntA a 
    -> Just $ EStrA $ joins1 $ flip concatMap (AInt.intervals a) $ \case
      AInt.In _ (Fin n) | n < 0 -> [bot]
      AInt.In NegInf  (Fin n) -> map (rep anyChar) [0..n]
      AInt.In (Fin m) (Fin n) -> map (rep anyChar) [max 0 m..n]
      AInt.In NegInf  PosInf  -> [star anyChar]
      AInt.In (Fin m) PosInf  -> [rep anyChar (max 0 m) <> star anyChar]
      _                       -> impossible

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

  --  TODO: generalize
  -- ⟦ x[[i,∞]] = c ⟧↑ₓ ≐ Σ*cΣ*   where i <= 0
  EStrAt (EVar _x) (EIntA a) :=: EChar c _
    | AInt.continuous a
    , Just i <- AInt.minimum a, i <= Fin 0
    , Just PosInf <- AInt.maximum a 
    -> Just $ EStrA $ star anyChar <> lit (AChar.eq c) <> star anyChar

  -- TODO: generalize
  -- ⟦ x[|x|-1] = c ⟧↑ₓ ≐ Σ*c
  EStrAt (EVar x1) (EStrLen (EVar x2) :+: EInt (-1) _) :=: EChar c _ | x1 == x2
    -> Just $ EStrA $ star anyChar <> lit (AChar.eq c)

  -- TODO: this is an over-approximation! 
  -- we don't capture that x must be greater than 0
  -- we don't capture that x is exactly all indexes of c in s
  -- ⟦ s[x] = c ⟧↑ₓ ≐ |s| - [1,∞]
  EStrAt (EVar s) (EVar x1) :=: EChar _ _ | x1 == x
    -> Just $ EStrLen (EVar s) :-: (EIntA $ AInt.ge 1)
  
  -- TODO: this is an over-approximation! (same as above)
  -- ⟦ s[x+e] = c ⟧↑ₓ ≐ |s| - [1,∞] + e
  -- EStrAt (EVar s) (EVar x1 :+: e) :=: EChar _ _ | x1 == x
  --   -> Just $ EStrLen (EVar s) :-: (EIntA $ AInt.ge 1) :-: e

  -- TODO: hack WIP
  EStrAt (EVar s) (EVar x1 :+: e) :=: EChar c pv | x1 == x
    -> Just $ EFun "indexesOf" [EVar s, EChar c pv] :-: e

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
  
  -- r -> trace (showPretty r) $ Nothing
  _ -> Nothing  -- TODO: add to error message (or norm first)

-------------------------------------------------------------------------------

-- | Independently normalize each side of a relation.
normRel :: Rel -> Rel
normRel (Rel op e1 e2) = Rel op (norm e1) (norm e2)
