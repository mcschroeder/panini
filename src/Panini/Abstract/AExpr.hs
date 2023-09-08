
-- TODO: remove
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Panini.Abstract.AExpr
  ( AExpr
  , pattern EBoolA
  , pattern EIntA
  , pattern EStrA
  , norm
  ) where

import Algebra.Lattice
import Control.Applicative
import Data.Generics.Uniplate.Operations qualified as Uniplate
import Panini.Abstract.ABool as ABool
import Panini.Abstract.AInt as AInt
import Panini.Abstract.AString as AString
import Panini.Abstract.AValue
import Panini.Provenance
import Panini.Syntax.Expressions
import Prelude
import Data.Text qualified as Text

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

instance PartialMeetSemilattice AExpr where  
  EAbs a ∧? EAbs b = EAbs <$> a ∧? b  
  EAbs a ∧? e      | containsTop a, eqTypeAE a e = Just e
  EAbs a ∧? e      | containsBot a, eqTypeAE a e = Just $ EAbs $ fillBot a
  e      ∧? EAbs a | containsTop a, eqTypeAE a e = Just e
  e      ∧? EAbs a | containsBot a, eqTypeAE a e = Just $ EAbs $ fillBot a
      
  EBool  a _ ∧? EBool  b _ = Just $ EBoolA $ ABool.eq a ∧ ABool.eq b
  EBool  a _ ∧? EBoolA b   = Just $ EBoolA $ ABool.eq a ∧ b
  EBoolA b   ∧? EBool  a _ = Just $ EBoolA $ ABool.eq a ∧ b
      
  EInt  a _ ∧? EInt  b _ = Just $ EIntA $ AInt.eq a ∧ AInt.eq b
  EInt  a _ ∧? EIntA b   = Just $ EIntA $ AInt.eq a ∧ b
  EIntA b   ∧? EInt  a _ = Just $ EIntA $ AInt.eq a ∧ b

  EStr a _ ∧? EStr  b _ = Just $ EStrA $ AString.eq (Text.unpack a) ∧ AString.eq (Text.unpack b)
  EStr a _ ∧? EStrA b   = Just $ EStrA $ AString.eq (Text.unpack a) ∧ b
  EStrA b  ∧? EStr  a _ = Just $ EStrA $ AString.eq (Text.unpack a) ∧ b

  (e1 :+: EIntA a) ∧? (e2 :+: EIntA b) | e1 == e2 = Just $ norm $ e1 :+: EIntA (a ∧ b)
  (e1 :+: EIntA a) ∧? (EIntA b :+: e2) | e1 == e2 = Just $ norm $ e1 :+: EIntA (a ∧ b)
  (e1 :+: EIntA a) ∧? e2               | e1 == e2 = Just $ norm $ e1 :+: EIntA (a ∧ AInt.eq 0)
  (EIntA a :+: e1) ∧? (e2 :+: EIntA b) | e1 == e2 = Just $ norm $ e1 :+: EIntA (a ∧ b)
  (EIntA a :+: e1) ∧? (EIntA b :+: e2) | e1 == e2 = Just $ norm $ e1 :+: EIntA (a ∧ b)
  (EIntA a :+: e1) ∧? e2               | e1 == e2 = Just $ norm $ e1 :+: EIntA (a ∧ AInt.eq 0)
  e1               ∧? (e2 :+: EIntA a) | e1 == e2 = Just $ norm $ e1 :+: EIntA (a ∧ AInt.eq 0)
  e1               ∧? (EIntA a :+: e2) | e1 == e2 = Just $ norm $ e1 :+: EIntA (a ∧ AInt.eq 0)

  a ∧? b | a == b    = Just a
         | otherwise = Nothing

------------------------------------------------------------------------------

-- | Normalize an abstract expression by (partial) evaluation.
norm :: AExpr -> AExpr
norm = Uniplate.rewrite $ \case
  EVal _ -> Nothing
  EAbs _ -> Nothing

  ENot (EBool  a pv) -> Just $ EBool  (not a) pv
  ENot (EBoolA a)    -> Just $ EBoolA (neg a)

  EInt  a _ :+: EInt  b _ -> Just $ EInt (a + b) NoPV
  EInt  a _ :+: EIntA b   -> Just $ EIntA $ AInt.add (AInt.eq a) b
  EIntA a   :+: EInt  b _ -> Just $ EIntA $ AInt.add a (AInt.eq b)
  EIntA a   :+: EIntA b   -> Just $ EIntA $ AInt.add a b
  EInt  0 _ :+: e         -> Just e
  e         :+: EInt  0 _ -> Just e
  EIntA a   :+: e         | [0] <- AInt.values a -> Just e
  e         :+: EIntA a   | [0] <- AInt.values a -> Just e
  EIntA a   :+: _         | isBot a -> Just $ EIntA bot  
  _         :+: EIntA a   | isBot a -> Just $ EIntA bot

  -- re-associate addition to get more rewriting opportunities
  (e1 :+: e2) :+: e3 -> Just $ e1 :+: (e2 :+: e3)

  EInt  a _ :-: EInt  b _ -> Just $ EInt (a - b) NoPV
  EInt  a _ :-: EIntA b   -> Just $ EIntA $ AInt.sub (AInt.eq a) b
  EIntA a   :-: EInt  b _ -> Just $ EIntA $ AInt.sub a (AInt.eq b)
  EIntA a   :-: EIntA b   -> Just $ EIntA $ AInt.sub a b
  e         :-: EInt  0 _ -> Just e
  e         :-: EIntA a   | [0] <- AInt.values a -> Just e
  EIntA a   :-: _         | isBot a -> Just $ EIntA bot
  _         :-: EIntA a   | isBot a -> Just $ EIntA bot

  -- rewrite constant subtractions into additions
  e :-: EInt  a pv -> Just $ e :+: EInt (-a) pv
  e :-: EIntA a    -> Just $ e :+: (EIntA $ AInt.sub (AInt.eq 0) a)
  
  EStrLen (EStr s _) -> Just $ EInt (fromIntegral $ Text.length s) NoPV
  
  EStrAt (EStr s _) (EInt (fromIntegral -> i) _)
    | i < Text.length s 
    -> Just $ EStr (Text.pack [Text.index s i]) NoPV    
  
  EStrSub (EStr s _) (EInt (fromIntegral -> i) _) (EInt (fromIntegral -> j) _)
    | let n = Text.length s, i < n, j < n, i <= j
    -> Just $ EStr (Text.take (j - i + 1) $ Text.drop i s) NoPV

  _ -> Nothing
