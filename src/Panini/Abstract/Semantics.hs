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
  r | x ∉ leftSide r -> abstract x =<< converse r

  -- isolating x via arithmetic ---------------------------  
  Rel op (e1 :+: e2) e3 | x ∈ e1 -> abstract x $ Rel op e1 (e3 :-: e2)
                        | x ∈ e2 -> abstract x $ Rel op e1 (e3 :-: e1)
  Rel op (e1 :-: e2) e3 | x ∈ e1 -> abstract x $ Rel op e1 (e3 :+: e2)
                        | x ∈ e2 -> abstract x $ Rel op e2 (e1 :+: e3)

  -- simple constant relations: x ⋈ c ---------------------
  EVar _x :≠: EBool c _ -> Just $ EBool (not c) NoPV
  EVar _x :≠: EInt  c _ -> Just $ EIntA (AInt.ne c)
  EVar _x :>: EInt  c _ -> Just $ EIntA (AInt.gt c)
  EVar _x :≥: EInt  c _ -> Just $ EIntA (AInt.ge c)
  EVar _x :<: EInt  c _ -> Just $ EIntA (AInt.lt c)
  EVar _x :≤: EInt  c _ -> Just $ EIntA (AInt.le c)
  EVar _x :>: EIntA a   -> Just $ EIntA (AInt.gtA a)
  EVar _x :≥: EIntA a   -> Just $ EIntA (AInt.geA a)
  EVar _x :<: EIntA a   -> Just $ EIntA (AInt.ltA a)
  EVar _x :≤: EIntA a   -> Just $ EIntA (AInt.leA a)
  EVar _x :≠: EChar c _ -> Just $ EStrA (lit $ AChar.ne c)

  -- simple expression relations: x ⋈ e -------------------
  Rel op (EVar _x) e 
    | x ∈ e -> Nothing  -- x may only occur on left-hand side
    | Eq <- op -> Just $ norm e
    | Gt <- op -> Just $ norm (e :+: EIntA (AInt.gt 0))
    | Ge <- op -> Just $ norm (e :+: EIntA (AInt.ge 0))
    | Lt <- op -> Just $ norm (e :+: EIntA (AInt.lt 0))
    | Le <- op -> Just $ norm (e :+: EIntA (AInt.le 0))

  -- string length: |x| ⋈ n -------------------------------
  EStrLen (EVar _x) :=: EInt i _ -> Just $ EStrA $ rep anyChar i
  EStrLen (EVar _x) :≠: EInt i _ -> Just $ EStrA $ joins1 [rep anyChar (i - 1), rep anyChar (i + 1) <> star anyChar]
  EStrLen (EVar _x) :≥: EInt i _ -> Just $ EStrA $ rep anyChar i <> star anyChar  
  EStrLen (EVar _x) :>: EInt i _ -> Just $ EStrA $ rep anyChar (i + 1) <> star anyChar
  EStrLen (EVar _x) :<: EInt 0 _ -> Just $ EStrA bot
  
  -- TODO: find general solution, this only applies to |s| = [a,∞]
  EStrLen (EVar _x) :=: EIntA a 
    | Just (Fin i) <- AInt.minimum a
    , Just PosInf  <- AInt.maximum a
    , AInt.continuous a
    -> Just $ EStrA $ rep anyChar i <> star anyChar
  
  EStrLen (EVar _x) :≥: EIntA a -> case AInt.minimum (a ∧ AInt.ge 0) of
    Just (Fin i) -> Just $ EStrA $ rep anyChar i <> star anyChar
    _            -> Just $ EStrA bot  
  
  EStrLen (EVar _x) :<: EIntA a | AInt.minimum a < Just (Fin 0) -> Just $ EStrA bot

  -- string of character-at-index: x[i] ⋈ c ---------------
  EStrAt (EVar _x) (EInt i _) :=: EChar c _ -> Just $ EStrA $ rep anyChar i <> lit (AChar.eq c) <> star anyChar
  EStrAt (EVar _x) (EInt i _) :≠: EChar c _ -> Just $ EStrA $ rep anyChar i <> lit (AChar.ne c) <> star anyChar

  -- index of character-at-index: s[x] ⋈ c ----------------
  -- TODO: WARNING: these are over-approximations! (or are they?)
  EStrAt (EVar s) (EVar i) :=: EChar _ _ | x == i -> Just $ EStrLen (EVar s) :-: (EIntA $ AInt.gt 0)
  EStrAt (EVar s) (EVar i) :≠: EChar _ _ | x == i -> Just $ EStrLen (EVar s) :-: (EIntA $ AInt.gt 0)
  
  -- note: the following abstractions are over-fitting on tests 013 and 016
  -- TODO: generalize these / are they even correct?
  EStrSub (EVar _) (EInt 0 _) (EVar n) :=: EVar t | n == x -> Just $ EStrLen (EVar t) :-: EInt 1 NoPV
  e@(EStrSub (EVar _s) (EInt 0 _) (EStrLen (EVar t1))) :=: EVar t2 | t1 == x, t2 == x -> Just e
  EStrSub (EVar s) (EInt 0 _) (EStrLen (EVar _t)) :=: EStr y _ | s == x -> Just $ EStrA $ AString.eq (Text.unpack y) <> star anyChar -- TODO: what if |t| /= |y| ??
  EStrLen (EStrSub (EVar s) (EInt 0 _) _) :≥: EInt 0 _ | s == x -> Just $ EStrA $ star anyChar  -- TODO: over-approximation?

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
