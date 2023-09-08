
-- TODO: remove
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Panini.Abstract.AExpr where

import Algebra.Lattice
import Control.Applicative
import Data.Generics.Uniplate.Operations qualified as Uniplate
import Panini.Abstract.ABool as ABool
import Panini.Abstract.AInt as AInt
import Panini.Abstract.AValue
import Panini.Provenance
import Panini.Syntax.Expressions
import Prelude

------------------------------------------------------------------------------

-- | An abstract expression can contain abstract values and admits partial meets.
type AExpr = Expr

pattern EBoolA :: ABool -> AExpr
pattern EBoolA a = EAbs (ABool a)

pattern EIntA :: AInt -> AExpr
pattern EIntA a = EAbs (AInt a)

pattern EStrA :: AString -> AExpr
pattern EStrA a = EAbs (AString a)

------------------------------------------------------------------------------

-- TODO: we need to normalize expressions before meeting them

-- TODO: allow more expression meets
instance PartialMeetSemilattice AExpr where  
  EAbs a ∧? EAbs b = EAbs <$> a ∧? b  
  
  EBool  a _ ∧? EBool  b _ = Just $ EBoolA $ ABool.eq a ∧ ABool.eq b
  EBoolA a   ∧? EBool  b _ = Just $ EBoolA $ a          ∧ ABool.eq b
  EBool  a _ ∧? EBoolA b   = Just $ EBoolA $ ABool.eq a ∧ b

  EInt  a _ ∧? EInt  b _ = Just $ EIntA $ AInt.eq a ∧ AInt.eq b
  EIntA a   ∧? EInt  b _ = Just $ EIntA $ a         ∧ AInt.eq b
  EInt  a _ ∧? EIntA b   = Just $ EIntA $ AInt.eq a ∧ b

  a ∧? b | a == b    = Just a
         | otherwise = tryMeetE a b <|> tryMeetE b a

tryMeetE :: AExpr -> AExpr -> Maybe AExpr
tryMeetE (EAbs a) e
  | containsTop a, eqTypeAE a e = Just e
  | containsBot a, eqTypeAE a e = Just $ EAbs $ fillBot a
  
tryMeetE (EVar x) (EVar y :+: EAbs (AInt a))
  | x == y, AInt.concreteMember 0 a = Just $ EVar x
  | x == y                          = Just $ EAbs (AInt bot)
  
tryMeetE (EStrLen (EVar s1)) (EStrLen (EVar s2) :+: EIntA a)
  | s1 == s2, AInt.concreteMember 0 a = Just $ EStrLen (EVar s1)
  | s1 == s2                          = Just $ EIntA bot

tryMeetE e1 (e2 :-: EIntA a2)
  = e1 ∧? (e2 :+: EIntA (AInt.sub (AInt.eq 0) a2))

tryMeetE _ _ = Nothing

------------------------------------------------------------------------------

-- | Normalize an expression by partial evaluation.
norm :: AExpr -> AExpr
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
