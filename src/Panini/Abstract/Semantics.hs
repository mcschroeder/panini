module Panini.Abstract.Semantics 
  ( abstractVar
  , concretizeVar
  , topExpr
  ) where

import Algebra.Lattice
import Data.Generics.Uniplate.Operations qualified as Uniplate
import Data.Text qualified as Text
import Panini.Abstract.ABool
import Panini.Abstract.AChar
import Panini.Abstract.AInt
import Panini.Abstract.AString
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
  EIntA a   :+: EInt  b _ -> Just $ EAbs $ AInt $ aIntegerAddI a b
  EInt  a _ :+: EIntA b   -> Just $ EAbs $ AInt $ aIntegerAddI b a

  e         :-: EInt  0 _ -> Just e
  EInt  a _ :-: EInt  b _ -> Just $ EInt (a - b) NoPV
  -- EIntA a   :-: EInt  b _ -> Just $ EAbs $ AInt $ aIntegerSubI a b
  -- EInt  a   :-: EIntA b _ -> Just $ EAbs $ AInt $ aIntegerSubI b a
  -- TODO
  
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

  EVar _ :=: EBool c _ -> return $ EBoolA $ aBoolEq c
  EVar _ :≠: EBool c _ -> return $ EBoolA $ aBoolEq (not c)
    
  EVar _ :=: EInt c _ -> return $ EIntA $ aIntegerEq c
  EVar _ :≠: EInt c _ -> return $ EIntA $ aIntegerNe c
  EVar _ :>: EInt c _ -> return $ EIntA $ aIntegerGt c
  EVar _ :≥: EInt c _ -> return $ EIntA $ aIntegerGe c
  EVar _ :<: EInt c _ -> return $ EIntA $ aIntegerLt c
  EVar _ :≤: EInt c _ -> return $ EIntA $ aIntegerLe c

  EVar _ :=: EChar c _ -> return $ EStrA $ aStringLit (aCharEq c)
  EVar _ :≠: EChar c _ -> return $ EStrA $ aStringLit (aCharNe c)

  EVar _ :∈: EStrA s -> return $ EStrA s

  EStrAt (EVar _) (EInt i _) :=: EChar c _ ->
    return $ EStrA $ mconcat [ aStringRep aStringSigma i
                               , aStringLit (aCharEq c)
                               , aStringStar aStringSigma]  -- Σ^(i-1)cΣ*

  EStrAt (EVar _) (EInt i _) :≠: EChar c _ -> 
    return $ EStrA $ mconcat $ [ aStringRep aStringSigma i
                                 , aStringLit (aCharNe c)
                                 , aStringStar aStringSigma]


  -- TODO: find general solution, this only applies to |s| = [a,∞]
  EStrLen _ :=: EIntA i
      | Just (Fin a) <- aMinimum i
      , Just PosInf <- aMaximum i
      , aContinuous i
      -> return $ EStrA $ mconcat [ aStringRep aStringSigma a
                         , aStringStar aStringSigma ]


  EStrLen (EVar _) :=: EInt i _ -> return $ EStrA $ aStringRep aStringSigma i

  EStrLen (EVar _) :≠: EInt i _ ->
      return $ EStrA $ joins1  -- Σ^(i-1) | Σ^(i+1)Σ*
        [ aStringRep aStringSigma (i-1)
        , mconcat [ aStringRep aStringSigma i
                  , aStringStar aStringSigma]  
        ]

  EStrLen (EVar _) :≥: EInt i _ ->
      return $ EStrA $ mconcat [ aStringRep aStringSigma i
                      , aStringStar aStringSigma]  -- Σ^iΣ*

  EStrLen (EVar _) :≥: EIntA a ->
      case aMinimum (a ∧ aIntegerGe 0) of
        Just (Fin i) -> return $ EStrA $ mconcat [ aStringRep aStringSigma i
                                        , aStringStar aStringSigma]  -- Σ^iΣ*
        _ -> return $ EStrA bot

  EStrLen (EVar _) :>: EInt i _ ->
      return $ EAbs $ AString $ mconcat [ aStringRep aStringSigma (i + 1)
                               , aStringStar aStringSigma]  -- Σ^iΣ*                           

    -- TODO: hardcoded hack?
  EStrLen (EVar _) :<: EInt 0 _ -> return $ EStrA bot
  EStrLen (EVar _) :<: EIntA a
      | aMinimum a < Just (Fin 0) -> return $ EStrA bot
    
    -- TODO: ???? I don't know about these...
  EVar _ :=: EVar y -> return $ EVar y
  EVar _ :≠: EVar y -> return $ ENot (EVar y)

  EVar _ :=: e@(EStrLen (EVar _)) -> return e
    
  EVar _ :≠: e@(EStrLen (EVar _)) -> return $ e :+: (EIntA $ aIntegerNe 0)
  EVar _ :<: e@(EStrLen (EVar _)) -> return $ e :-: (EIntA $ aIntegerGe 1)
  EVar _ :>: e@(EStrLen (EVar _)) -> return $ e :+: (EIntA $ aIntegerGe 1)
  EVar _ :≥: e@(EStrLen (EVar _)) -> return $ e :+: (EIntA $ aIntegerGe 0)


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
