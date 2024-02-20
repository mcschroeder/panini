
-- TODO: remove
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Panini.Abstract.AExpr
  ( AExpr
  , pattern EUnitA
  , pattern EBoolA
  , pattern EIntA
  , pattern EStrA
  , pattern ECharA  
  , topExpr
  , botExpr
  , normExpr
  ) where

import Algebra.Lattice
import Control.Applicative
import Data.Generics.Uniplate.Operations qualified as Uniplate
import Panini.Abstract.AUnit as AUnit
import Panini.Abstract.ABool as ABool
import Panini.Abstract.AInt as AInt
import Panini.Abstract.AChar as AChar
import Panini.Abstract.AString as AString
import Panini.Abstract.AValue as AValue
import Panini.Provenance
import Panini.Syntax.Expressions
import Panini.Syntax.Primitives
import Prelude
import Data.Text qualified as Text

------------------------------------------------------------------------------

-- | An abstract expression can contain abstract values and admits partial meets.
type AExpr = Expr

pattern EUnitA :: AUnit -> AExpr
pattern EUnitA a <- (toAUnit -> Just a) where
  EUnitA a = EAbs (AUnit a)

toAUnit :: AExpr -> Maybe AUnit
toAUnit = \case
  EAbs (AUnit a) -> Just a
  EUnit _        -> Just (AUnit.Unit)
  _              -> Nothing

pattern EBoolA :: ABool -> AExpr
pattern EBoolA a <- (toABool -> Just a) where
  EBoolA a = EAbs (ABool a)

toABool :: AExpr -> Maybe ABool
toABool = \case
  EAbs (ABool a) -> Just a
  EBool b _      -> Just (ABool.eq b)
  _              -> Nothing

pattern EIntA :: AInt -> AExpr
pattern EIntA a <- (toAInt -> Just a) where
  EIntA a = EAbs (AInt a)

toAInt :: AExpr -> Maybe AInt
toAInt = \case
  EAbs (AInt a) -> Just a
  EInt n _      -> Just (AInt.eq n)
  _             -> Nothing

pattern EStrA :: AString -> AExpr
pattern EStrA a <- (toAString -> Just a) where
  EStrA a = EAbs (AString a)

toAString :: AExpr -> Maybe AString
toAString = \case
  EAbs (AString a) -> Just a
  EStr s _         -> Just (AString.eq $ Text.unpack s)
  _                -> Nothing

pattern ECharA :: AChar -> AExpr
pattern ECharA a <- (toAChar -> Just a)  where
  ECharA a = EAbs (AString (lit a))

toAChar :: AExpr -> Maybe AChar
toAChar = \case
  EAbs (AString s) -> AString.toChar s
  EChar c _        -> Just $ AChar.eq c
  _                -> Nothing

topExpr :: Base -> AExpr
topExpr TUnit   = EUnitA top
topExpr TBool   = EBoolA top
topExpr TInt    = EIntA top
topExpr TString = EStrA top

botExpr :: Base -> AExpr
botExpr TUnit   = EUnitA bot
botExpr TBool   = EBoolA bot
botExpr TInt    = EIntA bot
botExpr TString = EStrA bot

------------------------------------------------------------------------------

instance PartialMeetSemilattice AExpr where  
  EAbs a ∧? EAbs b = EAbs <$> a ∧? b  
  EAbs a ∧? e      | containsTop a, eqTypeAE a e = Just e
  EAbs a ∧? e      | containsBot a, eqTypeAE a e = Just $ EAbs $ fillBot a
  e      ∧? EAbs a | containsTop a, eqTypeAE a e = Just e
  e      ∧? EAbs a | containsBot a, eqTypeAE a e = Just $ EAbs $ fillBot a
  
  ECon a ∧? ECon b = EAbs <$> fromValue a ∧? fromValue b
  ECon a ∧? EAbs b = EAbs <$> fromValue a ∧? b
  EAbs a ∧? ECon b = EAbs <$> fromValue b ∧? a

  (x :+: a) ∧? (y :+: b) | x == y = (x :+:) <$> a ∧? b
  (a :+: x) ∧? (y :+: b) | x == y = (x :+:) <$> a ∧? b
  (x :+: a) ∧? (b :+: y) | x == y = (x :+:) <$> a ∧? b
  (a :+: x) ∧? (b :+: y) | x == y = (x :+:) <$> a ∧? b
  (x :-: a) ∧? (y :-: b) | x == y = (x :-:) <$> a ∧? b
  
  (x      ) ∧? (y :+: b) | x == y = (x :+:) <$> EIntA (AInt.eq 0) ∧? b
  (x      ) ∧? (b :+: y) | x == y = (x :+:) <$> EIntA (AInt.eq 0) ∧? b
  (x      ) ∧? (y :-: b) | x == y = (x :-:) <$> EIntA (AInt.eq 0) ∧? b
  (x :+: a) ∧? (y      ) | x == y = (x :+:) <$> a ∧? EIntA (AInt.eq 0)
  (a :+: x) ∧? (y      ) | x == y = (x :+:) <$> a ∧? EIntA (AInt.eq 0)
  (x :-: a) ∧? (y      ) | x == y = (x :-:) <$> a ∧? EIntA (AInt.eq 0)

  a ∧? b | a == b    = Just a
         | otherwise = Nothing

------------------------------------------------------------------------------

-- | Normalize an abstract expression by (partial) evaluation. Note that this
-- operation will never introduce any abstract values; if the input expression
-- did not contain anything abstract, then neither will the normalized output.
normExpr :: AExpr -> AExpr
normExpr = Uniplate.rewrite $ \case

  ENot (EBool  a pv) -> Just $ EBool  (not a) pv
  ENot (EBoolA a)    -> Just $ EBoolA (neg a)

  EInt  a _ :+: EInt  b _ -> Just $ EInt (a + b) NoPV
  EIntA a   :+: EIntA b   -> Just $ EIntA $ AInt.add a b
  EInt  0 _ :+: e         -> Just e
  e         :+: EInt  0 _ -> Just e
  EIntA a   :+: e         | [0] <- AInt.values a -> Just e
  e         :+: EIntA a   | [0] <- AInt.values a -> Just e
  EIntA a   :+: _         | isBot a -> Just $ EIntA bot  
  _         :+: EIntA a   | isBot a -> Just $ EIntA bot

  EInt  a _ :-: EInt  b _ -> Just $ EInt (a - b) NoPV
  EIntA a   :-: EIntA b   -> Just $ EIntA $ AInt.sub a b
  e         :-: EInt  0 _ -> Just e
  e         :-: EIntA a   | [0] <- AInt.values a -> Just e
  EIntA a   :-: _         | isBot a -> Just $ EIntA bot
  _         :-: EIntA a   | isBot a -> Just $ EIntA bot

  -- re-associate addition/subtraction to get more rewriting opportunities
  (e1 :+: e2) :+: e3 -> Just $ e1 :+: (e2 :+: e3)
  (e1 :+: e2) :-: e3 -> Just $ e1 :+: (e2 :-: e3)  
  (e1 :-: e2) :+: e3 -> Just $ e1 :-: (e2 :-: e3)
  (e1 :-: e2) :-: e3 -> Just $ e1 :-: (e2 :+: e3)

  EStrLen (EStr s _) -> Just $ EInt (fromIntegral $ Text.length s) NoPV

  EStrLen (EStrA a) | isTop a -> Just $ EIntA $ AInt.ge 0
  
  EStrAt (EStr s _) (EInt (fromIntegral -> i) _)
    | i < Text.length s 
    -> Just $ EStr (Text.pack [Text.index s i]) NoPV    
  
  EStrSub (EStr s _) (EInt (fromIntegral -> i) _) (EInt (fromIntegral -> j) _)
    | let n = Text.length s, i < n, j < n, i <= j
    -> Just $ EStr (Text.take (j - i + 1) $ Text.drop i s) NoPV

  StrComp (EStrA s) -> Just $ EStrA $ neg s

  _ -> Nothing
