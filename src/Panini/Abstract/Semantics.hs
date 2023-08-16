module Panini.Abstract.Semantics 
  ( abstractVar
  , concretizeVar
  , topExpr
  ) where

import Algebra.Lattice
import Data.Generics.Uniplate.Operations qualified as Uniplate
import Data.Text qualified as Text
import Panini.Abstract.ABool as ABool
import Panini.Abstract.AChar as AChar
import Panini.Abstract.AInt as AInt
import Panini.Abstract.AString as AString
import Panini.Abstract.AValue
import Panini.Error
import Panini.Monad
import Panini.Pretty
import Panini.Provenance
import Panini.Syntax
import Prelude

-- local notation -------------------------------------------------------------

(∈) :: Subable a => Name -> a -> Bool
x ∈ e = x `elem` freeVars e

(∉) :: Subable a => Name -> a -> Bool
x ∉ e = x `notElem` freeVars e

-------------------------------------------------------------------------------

-- | Normalize an expression by partial evaluation, eliminating or reducing any
-- constant sub-expressions.
norm :: Expr -> Expr
norm = Uniplate.rewrite $ \case
  ENot (EBool  a pv) -> Just $ EBool  (not a) pv
  ENot (EBoolA a)    -> Just $ EBoolA (neg a)
  
  EInt  0 _ :+: e         -> Just e
  e         :+: EInt  0 _ -> Just e  
  EInt  a _ :+: EInt  b _ -> Just $ EInt (a + b) NoPV  
  EIntA a   :+: EInt  b _ -> Just $ EAbs $ AInt $ AInt.addI a b
  EInt  a _ :+: EIntA b   -> Just $ EAbs $ AInt $ AInt.addI b a

  e         :-: EInt  0 _ -> Just e
  EInt  a _ :-: EInt  b _ -> Just $ EInt (a - b) NoPV
  EIntA a   :-: EInt  b _ -> Just $ EAbs $ AInt $ AInt.subI a b
  EInt  a _ :-: EIntA b   -> Just $ EAbs $ AInt $ AInt.iSub a b
  
  EStrLen (EStr s _) -> Just $ EInt (fromIntegral $ Text.length s) NoPV
  
  EStrAt (EStr s _) (EInt i _) -> 
    Just $ EStr (Text.singleton $ Text.index s (fromIntegral i)) NoPV

  EStrSub (EStr s _) (EInt i _) (EInt j _) ->
    Just $ EStr (Text.take (fromIntegral $ j - i + 1) $ Text.drop (fromIntegral i) s) NoPV

  -- TODO: normalize more operations

  _ -> Nothing

-- | Independently normalize each side of a relation.
normRel :: Rel -> Rel
normRel (Rel op e1 e2) = Rel op (norm e1) (norm e2)

-------------------------------------------------------------------------------

-- | Rearrange the relation so that the given variable is the only variable on
-- the left-hand side, or as close to that as possible.
--
-- For example, rewrites @x + a > b@ to @x > b - a@.
isolate :: Name -> Rel -> Rel
isolate x = \case
  r | x ∉ r -> r
  r | x ∉ leftSide r -> maybe r (isolate x) (converse r)

  Rel op (e1 :+: e2) e3 
    | x ∈ e1 -> isolate x $ Rel op e1 $ norm (e3 :-: e2)
    | x ∈ e2 -> isolate x $ Rel op e2 $ norm (e3 :-: e1)  
  
  Rel op (e1 :-: e2) e3 
    | x ∈ e1 -> isolate x $ Rel op e1 $ norm (e3 :+: e2)
    | x ∈ e2 -> isolate x $ Rel op e2 $ norm (e1 :+: e3)

  r -> r

-------------------------------------------------------------------------------

-- | Abstract semantics of constrained variables (the ⟦⋅⟧↑⋅ function from the
-- paper). Essentially returns the (abstract) value of the given variable as
-- defined by the relation.
abstractVar :: Name -> Base -> Rel -> Pan Expr
abstractVar x b r0 = case isolate x (normRel r0) of
  r | x ∉ r -> return $ topExpr b

  EVar _ :=: EBool c _ -> return $ EBoolA $ ABool.eq c
  EVar _ :≠: EBool c _ -> return $ EBoolA $ ABool.eq (not c)
    
  EVar _ :=: EInt c _ -> return $ EIntA $ AInt.eq c
  EVar _ :≠: EInt c _ -> return $ EIntA $ AInt.ne c
  EVar _ :>: EInt c _ -> return $ EIntA $ AInt.gt c
  EVar _ :≥: EInt c _ -> return $ EIntA $ AInt.ge c
  EVar _ :<: EInt c _ -> return $ EIntA $ AInt.lt c
  EVar _ :≤: EInt c _ -> return $ EIntA $ AInt.le c

  EVar _ :=: EAbs a -> return $ EAbs a

  EVar _ :>: EIntA a -> return $ EIntA $ AInt.gtA a
  EVar _ :≥: EIntA a -> return $ EIntA $ AInt.geA a

  EVar _ :=: EChar c _ -> return $ EStrA $ lit (AChar.eq c)
  EVar _ :≠: EChar c _ -> return $ EStrA $ lit (AChar.ne c)

  EVar _ :=: EStr s _ -> return $ EStrA $ AString.eq $ Text.unpack s

  EVar _ :∈: EStrA s -> return $ EStrA s

  EStrAt (EVar _) (EInt i _) :=: EChar c _ -> return $ EStrA $ rep anyChar i <> lit (AChar.eq c) <> star anyChar
  EStrAt (EVar _) (EInt i _) :≠: EChar c _ -> return $ EStrA $ rep anyChar i <> lit (AChar.ne c) <> star anyChar

  -- TODO: WARNING: these are over-approximations! (or are they?)
  EStrAt (EVar s) (EVar i) :=: EChar _ _ | x == i -> return $ EStrLen (EVar s) :-: (EIntA $ AInt.gt 0)
  EStrAt (EVar s) (EVar i) :≠: EChar _ _ | x == i -> return $ EStrLen (EVar s) :-: (EIntA $ AInt.gt 0)

  EStrLen (EVar _) :=: EInt i _ -> return $ EStrA $ rep anyChar i
  EStrLen (EVar _) :≠: EInt i _ -> return $ EStrA $ joins1 [rep anyChar (i - 1), rep anyChar (i + 1) <> star anyChar]
  EStrLen (EVar _) :≥: EInt i _ -> return $ EStrA $ rep anyChar i <> star anyChar  
  EStrLen (EVar _) :>: EInt i _ -> return $ EStrA $ rep anyChar (i + 1) <> star anyChar
  EStrLen (EVar _) :<: EInt 0 _ -> return $ EStrA bot
  -- TODO EStrLen (EVar _) :≤: EInt 0 _ -> return $ EStrA ?

  -- TODO: find general solution, this only applies to |s| = [a,∞]
  EStrLen (EVar _) :=: EIntA i 
    | Just (Fin a) <- AInt.minimum i, Just PosInf <- AInt.maximum i, AInt.continuous i
    -> return $ EStrA $ rep anyChar a <> star anyChar

  EStrLen (EVar _) :≥: EIntA a -> case AInt.minimum (a ∧ AInt.ge 0) of
    Just (Fin i) -> return $ EStrA $ rep anyChar i <> star anyChar
    _            -> return $ EStrA bot
  
  EStrLen (EVar _) :<: EIntA a | AInt.minimum a < Just (Fin 0) -> return $ EStrA bot

    -- TODO: ???? I don't know about these...
  EVar _ :=: EVar y -> return $ EVar y
  EVar _ :≠: EVar y -> return $ ENot (EVar y)

  EVar _ :>: EVar y -> return $ EVar y :+: (EIntA $ AInt.gt 0)
  EVar _ :≥: EVar y -> return $ EVar y :+: (EIntA $ AInt.ge 0)
  EVar _ :<: EVar y -> return $ EVar y :+: (EIntA $ AInt.lt 0)
  EVar _ :≤: EVar y -> return $ EVar y :+: (EIntA $ AInt.le 0)

  EVar _ :=: e@(EStrLen (EVar _)) -> return e
    
  EVar _ :≠: e@(EStrLen (EVar _)) -> return $ e :+: (EIntA $ AInt.ne 0)
  EVar _ :<: e@(EStrLen (EVar _)) -> return $ e :-: (EIntA $ AInt.ge 1)
  EVar _ :>: e@(EStrLen (EVar _)) -> return $ e :+: (EIntA $ AInt.ge 1)
  EVar _ :≥: e@(EStrLen (EVar _)) -> return $ e :+: (EIntA $ AInt.ge 0)

  EVar _ :=: e@(EStrAt (EVar _) (EVar _)) -> return e

    -- TODO: ???? I don't know about these...
  EVar _ :=: e@(EStrAt (EVar _) (ECon _)) -> return $ e       -- x = s[i]
  EVar _ :≠: e@(EStrAt (EVar _) (ECon _)) -> return $ ENot e  -- x ≠ s[i]

  -- TODO: generalize this sort of thing
  EVar _ :=: e@(EVar _ :-: EInt _ _) -> return e

  r -> throwError $ AbstractionImpossible r x

topExpr :: Base -> Expr
topExpr TBool   = EAbs $ ABool top
topExpr TInt    = EAbs $ AInt top
topExpr TString = EAbs $ AString top
topExpr b       = panic $ "no" <+> symTop <+> "for " <+> pretty b

concretizeVar :: Name -> Expr -> Pan Rel
concretizeVar x e = case e of
  EStrA s -> return $ EVar x :∈: EStrA s  
  _ -> throwError $ ConcretizationImpossible e x


-- TODO: make this unnecessary
pattern EChar :: Char -> PV -> Expr
pattern EChar c pv <- ECon (S (Text.unpack -> [c]) pv)
