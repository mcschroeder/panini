{-# LANGUAGE OverloadedLists #-}
module Panini.Abstract.Semantics where

import Algebra.Lattice
import Control.Monad
import Data.Generics.Uniplate.Operations as Uniplate
import Data.Maybe
import Data.Text qualified as Text
import Panini.Abstract.ABool as ABool
import Panini.Abstract.AChar as AChar
import Panini.Abstract.AInt as AInt
import Panini.Abstract.AString as AString
import Panini.Abstract.AUnit as AUnit
import Panini.Abstract.AValue
import Panini.Error
import Panini.Monad
import Panini.Panic
import Panini.Pretty
import Panini.Provenance
import Panini.Regex qualified as Regex
import Panini.Regex.POSIX.ERE qualified as Regex.POSIX.ERE
import Panini.Syntax
import Prelude

--import Debug.Trace
trace :: String -> a -> a
trace _ = id

-------------------------------------------------------------------------------

-- | Normalize an expression by (partial) evaluation. 
-- 
-- While this function will never change the base type of an expression, it
-- might change its abstractness. In particular, it might evaluate a concrete
-- expression into bottom (e.g., ""[0] ⇝ ∅) or simplify a singleton abstract
-- value into its concrete counterpart (e.g., [0,0] ⇝ 0).
normExpr :: Expr -> Expr
-- normExpr = \case
normExpr e0 = trace ("normExpr " ++ showPretty e0) $ case e0 of
  -----------------------------------------------------------
  EUnitA Unit                                 -> EUnit   NoPV
  EBoolA a | Just c <- ABool.value    a       -> EBool c NoPV
  EIntA  a | [c]    <- AInt.values    a       -> EInt  c NoPV
  ECharA a | [c]    <- AChar.values   a       -> EChar c NoPV
--EStrA  a | [s]    <- AString.values a       -> EStr  s NoPV
  EStrA  a | AString.isEmpty a                -> EStr "" NoPV
  -----------------------------------------------------------
  ESol x b r | r' <- normRel r, r' /= r       -> normExpr $ ESol x b r'
  ESol x b r | Just e <- abstract x b r       -> e
  -----------------------------------------------------------
--EFun _ es | any hasBot (universeBi =<< es)  -> botExpr ??
  -----------------------------------------------------------
  ENot (EBool a pv)                           -> EBool (not a) pv
  ENot (EBoolA a)                             -> normExpr $ EBoolA (neg a)
  ENot (ENot e)                               -> normExpr $ e
  -----------------------------------------------------------
  EIntA BOT :+: _                             -> EIntA BOT
  _         :+: EIntA BOT                     -> EIntA BOT
  EInt  a _ :+: EInt  b _                     -> EInt (a + b) NoPV
  EIntA a   :+: EIntA b                       -> normExpr $ EIntA $ AInt.add a b
  EIntA a   :+: EInt  b _                     -> normExpr $ EIntA $ AInt.add a (AInt.eq b)
  EInt  a _ :+: EIntA b                       -> normExpr $ EIntA $ AInt.add (AInt.eq a) b
  a         :+: EInt  0 _                     -> normExpr $ a
  a         :+: EIntA AInt0                   -> normExpr $ a
  a         :+: EInt  b pv | b <= 0           -> normExpr $ a :-: EInt (negate b) pv
  a         :+: b          | a > b            -> normExpr $ b :+: a
  (a :+: b) :+: c          | (b ⏚), (c ⏚)    -> normExpr $ a :+: (normExpr $ b :+: c)
  (a :-: b) :+: c          | (b ⏚), (c ⏚)    -> normExpr $ a :-: (normExpr $ b :-: c)
  -----------------------------------------------------------
  EIntA BOT :-: _                             -> EIntA BOT
  _         :-: EIntA BOT                     -> EIntA BOT
  EInt  a _ :-: EInt  b _                     -> EInt (a - b) NoPV
  EIntA a   :-: EIntA b                       -> normExpr $ EIntA $ AInt.sub a b
  EIntA a   :-: EInt  b _                     -> normExpr $ EIntA $ AInt.sub a (AInt.eq b)
  EInt  a _ :-: EIntA b                       -> normExpr $ EIntA $ AInt.sub (AInt.eq a) b
  a         :-: EInt  0 _                     -> normExpr $ a
  a         :-: EIntA AInt0                   -> normExpr $ a
  a         :-: EInt  b pv | b <= 0           -> normExpr $ a :+: EInt (negate b) pv
  (a :-: b) :-: c          | (b ⏚), (c ⏚)    -> normExpr $ a :-: (normExpr $ b :+: c)
  (a :+: b) :-: c          | (b ⏚), (c ⏚)    -> normExpr $ a :+: (normExpr $ b :-: c)
  (a :+: b) :-: c          | (a ⏚), (c ⏚)    -> normExpr $ b :+: (normExpr $ a :-: c)
  -----------------------------------------------------------
  EMod (EInt a _) (EInt b _)                  -> EInt (a `mod` b) NoPV
  -----------------------------------------------------------
  EStrLen (EStr s _)                          -> EInt (fromIntegral $ Text.length s) NoPV
  EStrLen (EStrA a) | isTop a                 -> EIntA (AInt.ge 0)
  EStrLen (EStrA a) | Just n <- strLen1 a     -> EInt n NoPV
  -- NOTE: We don't have any efficient way to compute nor represent, in general,
  -- the precise lengths of all strings contained in an abstract string.
  -----------------------------------------------------------
  EStrAt (EStr s _) (EInt  i _)               -> normExpr $ ECharA $ charAt s (AInt.eq i)
  EStrAt (EStr s _) (EIntA i)                 -> normExpr $ ECharA $ charAt s i
  -----------------------------------------------------------
  EStrSub (EStr s _) (EInt  i _) (EInt  j _)  -> normExpr $ EStrA $ strSub s (AInt.eq i) (AInt.eq j)
  EStrSub (EStr s _) (EIntA i  ) (EIntA j  )  -> normExpr $ EStrA $ strSub s i j
  EStrSub (EStr s _) (EIntA i  ) (EInt  j _)  -> normExpr $ EStrA $ strSub s i (AInt.eq j)
  EStrSub (EStr s _) (EInt  i _) (EIntA j  )  -> normExpr $ EStrA $ strSub s (AInt.eq i) j
  EStrSub s1 (EInt 0 _) (EStrLen s2 :-: EInt 1 _) | s1 == s2 -> normExpr s1
  -----------------------------------------------------------
  -- NOTE: We want to defer resolution of EStrComp as long as possible, 
  -- in order to exploit opportunities for double-negation cancellation!   
  -- EStrComp (EStr s _)                      -> normExpr $ EStrA (neg $ AString.eq $ Text.unpack s) 
  -- EStrComp (EStrA s)                       -> normExpr $ EStrA $ neg s
  EStrComp (EStrComp e)                       -> normExpr $ e  
  -----------------------------------------------------------
  EStrConc (EStr  a _) (EStr  b _)            -> normExpr $ EStr (a <> b) NoPV
  EStrConc (EStrA a  ) (EStr  b _)            -> normExpr $ EStrA (a <> AString.eq (Text.unpack b))
  EStrConc (EStr  a _) (EStrA b  )            -> normExpr $ EStrA (AString.eq (Text.unpack a) <> b)
  EStrConc (EStrA a  ) (EStrA b  )            -> normExpr $ EStrA (a <> b)  
  -----------------------------------------------------------
  EStrStar (EStr  s _)                        -> normExpr $ EStrA $ star (AString.eq (Text.unpack s))
  EStrStar (EStrA s  )                        -> normExpr $ EStrA $ star s
  -----------------------------------------------------------
  EStrContains (EStr s _) (EStr t _)          -> EBool (t `Text.isInfixOf` s) NoPV
  EStrContains (EStr s _) (EStrA t )          -> EBool (AString.member (Text.unpack s) t) NoPV
  -----------------------------------------------------------
  e | e' <- descend normExpr e, e' /= e       -> normExpr e'
    | otherwise                               -> e

-------------------------------------------------------------------------------

-- | The canonical tautological relation. 
--
-- To find out if a relation is a tautology, normalize it first using 'normRel'
-- and then compare it with 'taut'. (But note that if a comparison with 'taut'
-- returns 'False', the relation could still be a tautology. Not all
-- tautological relations necessarily normalize.)
taut :: Rel
taut = EUnit NoPV :=: EUnit NoPV

-- | The canonical contradictory relation; see 'taut'.
cont :: Rel
cont = EUnit NoPV :≠: EUnit NoPV

toRel :: Bool -> Rel
toRel True = taut
toRel False = cont

-- | Normalize a relation by (partial) evaluation.
--
-- This function might change both the abstractness and the base type of a
-- relation's sub-expressions. A type change happens in particular when a
-- relation is evaluated to 'taut' or 'cont' (e.g., 2 > 1 ⇝ unit = unit). Note
-- that the relation itself continues to be a Boolean predicate, with the same
-- truth value as before.
normRel :: Rel -> Rel
-- normRel = \case
normRel r0 = trace ("normRel " ++ showPretty r0) $ case r0 of
  -----------------------------------------------------------
  EUnit   _ :=: EUnit    _                    -> taut
  EBool a _ :=: EBool  b _                    -> toRel (a == b)
  EInt  a _ :=: EInt   b _                    -> toRel (a == b)
  EChar a _ :=: EChar  b _                    -> toRel (a == b)
  EStr  a _ :=: EStr   b _                    -> toRel (a == b)
  EUnit   _ :=: EUnitA b                      -> toRel (b == Unit)
  EBool a _ :=: EBoolA b                      -> toRel (ABool.member a b)
  EInt  a _ :=: EIntA  b                      -> toRel (AInt.member a b)
  EChar a _ :=: ECharA b                      -> toRel (AChar.member a b)
  EStr  a _ :=: EStrA  b                      -> toRel (AString.member (Text.unpack a) b)
  EAbs  a   :=: EAbs   b   | Just m <- a ∧? b -> toRel (not $ hasBot m)
  a         :=: b          | a == b           -> taut
  -----------------------------------------------------------
  EUnit   _ :≠: EUnit    _                    -> cont
  EBool a _ :≠: EBool  b _                    -> toRel (a /= b)
  EInt  a _ :≠: EInt   b _                    -> toRel (a /= b)
  EChar a _ :≠: EChar  b _                    -> toRel (a /= b)
  EStr  a _ :≠: EStr   b _                    -> toRel (a /= b)
  EUnit   _ :≠: EUnitA b                      -> toRel (b /= Unit)
  EBool a _ :≠: EBoolA b                      -> toRel (not $ ABool.member a b)
  EInt  a _ :≠: EIntA  b                      -> toRel (not $ AInt.member a b)
  EChar a _ :≠: ECharA b                      -> toRel (not $ AChar.member a b)
  EStr  a _ :≠: EStrA  b                      -> toRel (not $ AString.member (Text.unpack a) b)
  EAbs  a   :≠: EAbs   b   | Just m <- a ∧? b -> toRel (hasBot m)
  a         :≠: b          | a == b           -> cont
  -----------------------------------------------------------
  EInt  a _ :<: EInt   b _                    -> toRel (a <  b)
  EInt  a _ :≤: EInt   b _                    -> toRel (a <= b)
  EInt  a _ :>: EInt   b _                    -> toRel (a >  b)
  EInt  a _ :≥: EInt   b _                    -> toRel (a >= b)
  a         :<: b          | a == b           -> cont
  a         :≤: b          | a == b           -> taut
  a         :>: b          | a == b           -> cont
  a         :≥: b          | a == b           -> taut
  -----------------------------------------------------------
  -- NOTE: ">" is the structural ordering on 'Expr'; after 
  -- this block, the "smaller" expression will be on the LHS,
  -- with variables < functions < constants < abstract values
  a :=: b | a > b                             -> normRel $ b :=: a
  a :≠: b | a > b                             -> normRel $ b :≠: a
  a :<: b | a > b                             -> normRel $ b :>: a
  a :≤: b | a > b                             -> normRel $ b :≥: a
  a :>: b | a > b                             -> normRel $ b :<: a
  a :≥: b | a > b                             -> normRel $ b :≤: a
  -----------------------------------------------------------
  ENot a :=: ENot b                           -> normRel $ a :=: b
  ENot a :≠: ENot b                           -> normRel $ a :≠: b
  ENot a :=: b                                -> normRel $ a :≠: b
  ENot a :≠: b                                -> normRel $ a :=: b
  a      :=: ENot b                           -> normRel $ a :≠: b
  a      :≠: ENot b                           -> normRel $ a :=: b
  -----------------------------------------------------------
  a :=: (b :+: EInt  c _) | a == b            -> toRel (c == 0)
  a :≠: (b :+: EInt  c _) | a == b            -> toRel (c /= 0)
  a :<: (b :+: EInt  c _) | a == b            -> toRel (c >  0)
  a :≤: (b :+: EInt  c _) | a == b            -> toRel (c >= 0)
  a :>: (b :+: EInt  c _) | a == b            -> toRel (c <  0)
  a :≥: (b :+: EInt  c _) | a == b            -> toRel (c <= 0)
  a :=: (b :+: EIntA c  ) | a == b            -> toRel (AInt.member 0 c)
  a :≠: (b :+: EIntA c  ) | a == b            -> toRel (not $ AInt.member 0 c)
  a :<: (b :+: EIntA c  ) | a == b            -> toRel (not $ isBot $ c ∧ AInt.gt 0)
  a :≤: (b :+: EIntA c  ) | a == b            -> toRel (not $ isBot $ c ∧ AInt.ge 0)
  a :>: (b :+: EIntA c  ) | a == b            -> toRel (not $ isBot $ c ∧ AInt.lt 0)
  a :≥: (b :+: EIntA c  ) | a == b            -> toRel (not $ isBot $ c ∧ AInt.le 0)
  (b :+: EInt  c _) :=: a | a == b            -> toRel (c == 0)
  (b :+: EInt  c _) :≠: a | a == b            -> toRel (c /= 0)
  (b :+: EInt  c _) :>: a | a == b            -> toRel (c >  0)  
  (b :+: EInt  c _) :≥: a | a == b            -> toRel (c >= 0)
  (b :+: EInt  c _) :<: a | a == b            -> toRel (c <  0)
  (b :+: EInt  c _) :≤: a | a == b            -> toRel (c <= 0)
  (b :+: EIntA c  ) :=: a | a == b            -> toRel (AInt.member 0 c)
  (b :+: EIntA c  ) :>: a | a == b            -> toRel (not $ isBot $ c ∧ AInt.gt 0)
  (b :+: EIntA c  ) :≥: a | a == b            -> toRel (not $ isBot $ c ∧ AInt.ge 0)
  (b :+: EIntA c  ) :<: a | a == b            -> toRel (not $ isBot $ c ∧ AInt.lt 0)
  (b :+: EIntA c  ) :≤: a | a == b            -> toRel (not $ isBot $ c ∧ AInt.le 0)
  -----------------------------------------------------------
  a :=: (b :-: EInt  c _) | a == b            -> toRel (c == 0)
  a :≠: (b :-: EInt  c _) | a == b            -> toRel (c /= 0)
  a :<: (b :-: EInt  c _) | a == b            -> toRel (c <  0)
  a :≤: (b :-: EInt  c _) | a == b            -> toRel (c <= 0)
  a :>: (b :-: EInt  c _) | a == b            -> toRel (c >  0)
  a :≥: (b :-: EInt  c _) | a == b            -> toRel (c >= 0)
  a :=: (b :-: EIntA c  ) | a == b            -> toRel (AInt.member 0 c)
  a :≠: (b :-: EIntA c  ) | a == b            -> toRel (not $ AInt.member 0 c)
  a :<: (b :-: EIntA c  ) | a == b            -> toRel (not $ isBot $ c ∧ AInt.lt 0)
  a :≤: (b :-: EIntA c  ) | a == b            -> toRel (not $ isBot $ c ∧ AInt.le 0)
  a :>: (b :-: EIntA c  ) | a == b            -> toRel (not $ isBot $ c ∧ AInt.gt 0)
  a :≥: (b :-: EIntA c  ) | a == b            -> toRel (not $ isBot $ c ∧ AInt.ge 0)  
  (b :-: EInt  c _) :=: a | a == b            -> toRel (c == 0)
  (b :-: EInt  c _) :≠: a | a == b            -> toRel (c /= 0)
  (b :-: EInt  c _) :>: a | a == b            -> toRel (c <  0)
  (b :-: EInt  c _) :≥: a | a == b            -> toRel (c <= 0)
  (b :-: EInt  c _) :<: a | a == b            -> toRel (c >  0)
  (b :-: EInt  c _) :≤: a | a == b            -> toRel (c >= 0)
  (b :-: EIntA c  ) :=: a | a == b            -> toRel (AInt.member 0 c)
  (b :-: EIntA c  ) :≠: a | a == b            -> toRel (not $ AInt.member 0 c)
  (b :-: EIntA c  ) :>: a | a == b            -> toRel (not $ isBot $ c ∧ AInt.lt 0)
  (b :-: EIntA c  ) :≥: a | a == b            -> toRel (not $ isBot $ c ∧ AInt.le 0)
  (b :-: EIntA c  ) :<: a | a == b            -> toRel (not $ isBot $ c ∧ AInt.gt 0)
  (b :-: EIntA c  ) :≤: a | a == b            -> toRel (not $ isBot $ c ∧ AInt.ge 0)
  -----------------------------------------------------------
  a :<: (b :+: EInt 1 _)                      -> normRel $ a :≤: b
  a :≤: (b :-: EInt 1 _)                      -> normRel $ a :<: b
  a :>: (b :-: EInt 1 _)                      -> normRel $ a :≥: b
  a :≥: (b :+: EInt 1 _)                      -> normRel $ a :>: b
  -----------------------------------------------------------
  Rel op (a :+: b) c           | (a ⏚), (c ⏚) -> normRel $ Rel op b (normExpr $ c :-: a)
  Rel op (a :+: b) c           | (b ⏚), (c ⏚) -> normRel $ Rel op a (normExpr $ c :-: b)
  Rel op (a :+: b) c@(_ :+: d) | (a ⏚), (d ⏚) -> normRel $ Rel op b (normExpr $ c :-: a)
  Rel op (a :+: b) c@(_ :+: d) | (b ⏚), (d ⏚) -> normRel $ Rel op a (normExpr $ c :-: b)
  Rel op (a :+: b) c@(d :+: _) | (a ⏚), (d ⏚) -> normRel $ Rel op b (normExpr $ c :-: a)
  Rel op (a :+: b) c@(d :+: _) | (b ⏚), (d ⏚) -> normRel $ Rel op a (normExpr $ c :-: b)
  Rel op (a :+: b) c@(_ :-: d) | (a ⏚), (d ⏚) -> normRel $ Rel op b (normExpr $ c :-: a)
  Rel op (a :+: b) c@(_ :-: d) | (b ⏚), (d ⏚) -> normRel $ Rel op a (normExpr $ c :-: b)
  Rel op (a :+: b) c@(d :-: _) | (a ⏚), (d ⏚) -> normRel $ Rel op b (normExpr $ c :-: a)
  Rel op (a :+: b) c@(d :-: _) | (b ⏚), (d ⏚) -> normRel $ Rel op a (normExpr $ c :-: b)
  Rel op (a :-: b) c           | (a ⏚), (c ⏚) -> normRel $ Rel op b (normExpr $ a :-: c)
  Rel op (a :-: b) c           | (b ⏚), (c ⏚) -> normRel $ Rel op a (normExpr $ c :+: b)
  Rel op (a :-: b) c@(_ :+: d) | (a ⏚), (d ⏚) -> normRel $ Rel op b (normExpr $ a :-: c)
  Rel op (a :-: b) c@(_ :+: d) | (b ⏚), (d ⏚) -> normRel $ Rel op a (normExpr $ c :+: b)
  Rel op (a :-: b) c@(d :+: _) | (a ⏚), (d ⏚) -> normRel $ Rel op b (normExpr $ a :-: c)
  Rel op (a :-: b) c@(d :+: _) | (b ⏚), (d ⏚) -> normRel $ Rel op a (normExpr $ c :+: b)
  Rel op (a :-: b) c@(_ :-: d) | (a ⏚), (d ⏚) -> normRel $ Rel op b (normExpr $ a :-: c)
  Rel op (a :-: b) c@(_ :-: d) | (b ⏚), (d ⏚) -> normRel $ Rel op a (normExpr $ c :+: b)
  Rel op (a :-: b) c@(d :-: _) | (a ⏚), (d ⏚) -> normRel $ Rel op b (normExpr $ a :-: c)
  Rel op (a :-: b) c@(d :-: _) | (b ⏚), (d ⏚) -> normRel $ Rel op a (normExpr $ c :+: b)
  -----------------------------------------------------------
  EMod (EIntA a) (EInt b _) :=: EInt c _
    | any (\x -> x `mod` b == c) $ take 100 $ AInt.values a -> taut
  -----------------------------------------------------------
  EIntA î :=: ESol a1 TInt (EMod (EVar a2) (EInt n _) :=: EInt m _)
    | a1 == a2, n >= 0, m >= 0, AInt.ge 0 == î ∧ AInt.ge 0 -> taut
  -----------------------------------------------------------
  EStrComp a :=: EStrComp b                   -> normRel $ a :=: b
  EStrComp a :≠: EStrComp b                   -> normRel $ a :≠: b
  EStrComp a :=: b                            -> normRel $ a :≠: b
  EStrComp a :≠: b                            -> normRel $ a :=: b
  a          :≠: EStrComp b                   -> normRel $ a :=: b
  -----------------------------------------------------------
  (EStrFirstIndexOfChar s1 c :-: EIntA î) :=: EStrLen s2 -> normRel $ (EStrFirstIndexOfChar s1 c :+: EIntA (AInt.sub (AInt.eq 0) î)) :=: EStrLen s2
  -----------------------------------------------------------
  EStrIndexOf s c i :≠: EIntA n̂ -> normRel $ EStrIndexOf s c i :=: EIntA (neg n̂)
  -----------------------------------------------------------
  i1 :+: EIntA n̂ :=: EStrIndexOf s c i2 
    | i1 == i2, let n̂' = n̂ ∧ AInt.ge 0, n̂' /= n̂ 
    -> normRel $ i1 :+: EIntA n̂' :=: EStrIndexOf s c i2
  i1 :-: EIntA n̂ :=: EStrIndexOf s c i2 
    | i1 == i2, let n̂' = n̂ ∧ AInt.le 0, n̂' /= n̂ 
    -> normRel $ i1 :-: EIntA n̂' :=: EStrIndexOf s c i2
  -----------------------------------------------------------
  EStrLen s1 :+: EIntA n̂ :=: EStrIndexOf s2 c i
    | s1 == s2, let n̂' = n̂ ∧ AInt.lt 0, n̂' /= n̂ 
    -> normRel $ EStrLen s1 :+: EIntA n̂' :=: EStrIndexOf s2 c i
  -----------------------------------------------------------
  k :=: EStrIndexOf s t i | k == i 
    -> normRel $ EStrSub s i (i :+: (EStrLen t :-: EInt 1 NoPV)) :=: t
  -----------------------------------------------------------
  EStrLen s1 :-: EInt 1 _ :=: EStrIndexOf s2 (EStr t _) (EInt i _)
    | s1 == s2, [c] <- Text.unpack t
    , let ĉ = AChar.eq c, let c̄ = AChar.ne c    
    -> normRel $ s1 :=: EStrA (rep anyChar i <> star (lit c̄) <> lit ĉ)
  -----------------------------------------------------------
  EStrSub s i1 i2 :=: EStr t pv
    | i1 == i2, [c] <- t -> normRel $ EStrAt s i1 :=: EChar c pv
  EStrSub s i1 i2 :=: EStrA t
    | i1 == i2, Just c <- AString.toChar (t ∧ anyChar) -> normRel $ EStrAt s i1 :=: ECharA c
  -----------------------------------------------------------
  a :=: ESol x b r | Just r' <- tryEqARel a x b r -> normRel r'  
  a :≠: ESol x b r | Just r' <- tryNeARel a x b r -> normRel r'
  -----------------------------------------------------------
  r | [x] <- freeVars r
    , Just b <- typeOfVarInRel x r
    , Just e <- abstract x b r
    , let r' = EVar x :=: e
    , r' < r                                  -> normRel r'
  -----------------------------------------------------------
  r | r' <- descendBi normExpr r, r' /= r     -> normRel r'
    | otherwise                               -> r

isSol :: Expr -> Bool
isSol (ESol _ _ _) = True
isSol _            = False

pattern Range :: Inf Integer -> Inf Integer -> AInt
pattern Range a b <- (AInt.intervals -> [AInt.In a b])

-------------------------------------------------------------------------------

-- | Try to resolve equality between an expression and an abstract relation.
-- For example, @[1,∞] = {x| s[x] ≠ 'a'}@ resolves to @s[[1,∞]] = Σ∖a@.
tryEqARel :: Expr -> Name -> Base -> Rel -> Maybe Rel
tryEqARel a x b = \case
  r | ESol x1 b1 r1 <- a            -> tryEqARel2 (x1,b1,r1) (x,b,r)
  r | isConcrete a, x `notFreeIn` a -> Just $ subst a x r
  -----------------------------------------------------------
  EStrAt (EVar s) i :=: EChar  c pv -> Just $ EStrAt (EVar s) (subst a x i) :=: EChar  c pv
  EStrAt (EVar s) i :=: ECharA ĉ    -> Just $ EStrAt (EVar s) (subst a x i) :=: ECharA ĉ
  EStrAt (EVar s) i :≠: EChar  c _  -> Just $ EStrAt (EVar s) (subst a x i) :=: ECharA (AChar.ne c)
  EStrAt (EVar s) i :≠: ECharA ĉ    -> Just $ EStrAt (EVar s) (subst a x i) :=: ECharA (neg ĉ)
  -----------------------------------------------------------
  EVar x1 :≠: e | x == x1, x `notFreeIn` e -> Just $ a :≠: e
  (EVar x1 :-: EInt k pv) :≠: e | x == x1, x `notFreeIn` e -> Just $ (a :-: EInt k pv) :≠: e
  -----------------------------------------------------------
  _                                 -> Nothing

-- | Try to resolve inequality between an expressions and an abstract relation.
-- For example, @[1,∞] || {x| s[x] ≠ 'a'}@ resolves to @s[[1,∞]] ≠ Σ∖a@
tryNeARel :: Expr -> Name -> Base -> Rel -> Maybe Rel
tryNeARel a x b r = fmap inverse $ tryEqARel a x b r

pattern ECharA' :: AChar -> Expr
pattern ECharA' c <- (exprToAChar -> Just c)

exprToAChar :: Expr -> Maybe AChar
exprToAChar (EChar  c _) = Just $ AChar.eq c
exprToAChar (ECharA c  ) = Just c
exprToAChar _            = Nothing

pattern EqChar :: Expr -> AChar -> Rel
pattern EqChar e c <- (relToEqChar -> Just (e,c))

relToEqChar :: Rel -> Maybe (Expr, AChar)
relToEqChar = \case
   e :=: EChar  c _ -> Just (e, AChar.eq c)
   e :=: ECharA c   -> Just (e, c)
   e :≠: EChar  c _ -> Just (e, AChar.ne c)
   e :≠: ECharA c   -> Just (e, neg c)
   _ -> Nothing

pattern VarPlusN :: Name -> Integer -> Expr
pattern VarPlusN x n <- (exprToVarPlusN -> Just (x,n))

exprToVarPlusN :: Expr -> Maybe (Name, Integer)
exprToVarPlusN = \case
  EVar x -> Just (x, 0)
  EVar x :+: EInt n _ -> Just (x, n)
  _ -> Nothing

-- | Try to resolve equality between two abstract relations.
tryEqARel2 :: (Name,Base,Rel) -> (Name,Base,Rel) -> Maybe Rel
tryEqARel2 (x1,b1,r1) (x2,b2,r2) = case (r1,r2) of

  (EStrAt (EVar s1) (VarPlusN i1 n1) `EqChar` c1, 
   EStrAt (EVar s2) (VarPlusN i2 n2) `EqChar` c2)
   | b1 == b2, x1 == i1, x2 == i2, s1 == s2 
   , let n = n2 - n1
   , let t | n > 0 = star anyChar <> lit c1 <> rep anyChar (n-1) <> lit c2 <> star anyChar
           | n < 0 = star anyChar <> lit c2 <> rep anyChar (n-1) <> lit c1 <> star anyChar
           | otherwise = star anyChar <> lit (c1 ∧ c2) <> star anyChar
    -> Just $ EVar s1 :=: EStrA t

  -- TODO: generalize these hackily hardcoded rules
  (EStrAt (EVar s1) (EVar y1 :-: EIntA b) :=: ECharA cb,
   EStrAt (EVar s2) (EVar y2 :-: EIntA a) :=: ECharA ca)
   | b1 == b2, x1 == y1, x2 == y2
   , s1 == s2, (a ∧ AInt.ge 1) == AInt.ge 1, (b ∧ AInt.ge 1) == AInt.ge 2
   , let t1 = lit cb <> star anyChar <> lit ca
   , let t2 = lit ca <> star anyChar <> lit cb <> anyChar
   , let t3 = lit (ca ∧ cb) <> anyChar
   , let t = star anyChar <> (t1 ∨ t2 ∨ t3) <> star anyChar
   -> Just $ EVar s1 :=: EStrA t
  
  -- TODO: see above
  (EStrAt (EVar s1) (EVar y1 :-: EIntA b) :=: ECharA cb,
   EStrAt (EVar s2) (EVar y2 :-: EIntA a) :=: ECharA ca)
   | b1 == b2, x1 == y1, x2 == y2
   , s1 == s2, (a ∧ AInt.ge 1) == AInt.ge 1, (b ∧ AInt.ge 1) == AInt.ge 1
   , let t1 = lit cb <> star anyChar <> lit ca
   , let t2 = lit ca <> star anyChar <> lit cb
   , let t3 = lit (ca ∧ cb)
   , let t = star anyChar <> (t1 ∨ t2 ∨ t3) <> star anyChar
   -> Just $ EVar s1 :=: EStrA t

  -- TODO: see above
  (EStrAt (EVar s1) (EVar y1 :-: EIntA b) :=: ECharA cb,
   EStrAt (EVar s2) (EVar y2 :-: EIntA a) :=: ECharA ca)
   | b1 == b2, x1 == y1, x2 == y2
   , s1 == s2, (a ∧ AInt.ge 1) == AInt.ge 1, (b ∧ AInt.ge 1) == AInt.eq 1
   , let t2 = lit ca <> star anyChar <> lit cb
   , let t3 = lit (ca ∧ cb)
   , let t = star anyChar <> (t2 ∨ t3) <> star anyChar
   -> Just $ EVar s1 :=: EStrA t

  -- TODO: see above
  (EStrAt (EVar s1) (EVar y1 :+: EIntA a) :=: ECharA ca,
   EStrAt (EVar s2) (EVar y2 :-: EIntA b) :=: ECharA cb)
   | b1 == b2, x1 == y1, x2 == y2
   , s1 == s2, (a ∧ AInt.ge 0) == AInt.ge 0, (b ∧ AInt.ge 0) == AInt.ge 0   
   , let t1 = lit (ca ∧ cb)
   , let t2 = lit ca <> star anyChar <> lit cb
   , let t = star anyChar <> (t1 ∨ t2) <> star anyChar
   -> Just $ EVar s1 :=: EStrA t

  _ -> Nothing

-------------------------------------------------------------------------------

abstractVarToValue :: Name -> Base -> Rel -> Pan AValue
abstractVarToValue x b r0 = do
  let r = normRel r0
  let e = abstract x b r
  unless (isNothing e) $
    logMessage $ "⟦" <> pretty r0 <> "⟧↑" <> pretty x <+> "≐" <+> pretty e
  case e of
    Just (ECon c) -> return $ fromValue c
    Just (EAbs a) -> return a
    Just e'       -> throwError $ AbstractionToValueImpossible x r e'
    Nothing       -> throwError $ AbstractionImpossible x r

abstractVar :: Name -> Base -> Rel -> Pan Expr
abstractVar x b r0 = do
  let r = normRel r0
  let e = fromMaybe (ESol x b r) (abstract x b r)
  logMessage $ "⟦" <> pretty r0 <> "⟧↑" <> pretty x <+> "≐" <+> pretty e
  return e

abstract :: Name -> Base -> Rel -> Maybe Expr
-- abstract x b = \case
abstract x b r0 = trace ("abstract " ++ showPretty x ++ " " ++ showPretty r0 ++ " " ++ showPretty (freeVars r0)) $ case r0 of
  -----------------------------------------------------------
  r | x `notFreeIn` r                         -> Nothing
  -----------------------------------------------------------  
  e1 :=: e2 | x `notFreeIn` e1                -> abstract x b $ e2 :=: e1
  e1 :≠: e2 | x `notFreeIn` e1                -> abstract x b $ e2 :≠: e1
  e1 :<: e2 | x `notFreeIn` e1                -> abstract x b $ e2 :>: e1
  e1 :≤: e2 | x `notFreeIn` e1                -> abstract x b $ e2 :≥: e1
  e1 :>: e2 | x `notFreeIn` e1                -> abstract x b $ e2 :<: e1
  e1 :≥: e2 | x `notFreeIn` e1                -> abstract x b $ e2 :≤: e1
  -- NOTE: below here, x occurs on the LHS and may also occur on the RHS
  -----------------------------------------------------------
  -- TODO: this kind of reordering should happen during normRel, no?
  (EStrLen s2 :+: EInt  i _) :=: EStrFirstIndexOfChar s1 c -> abstract x b $ (EStrFirstIndexOfChar s1 c :+: EIntA (AInt.sub (AInt.eq 0) (AInt.eq i))) :=: EStrLen s2
  (EStrLen s2 :+: EIntA î  ) :=: EStrFirstIndexOfChar s1 c -> abstract x b $ (EStrFirstIndexOfChar s1 c :+: EIntA (AInt.sub (AInt.eq 0)          î )) :=: EStrLen s2
  (EStrLen s2 :-: EInt  i _) :=: EStrFirstIndexOfChar s1 c -> abstract x b $ (EStrFirstIndexOfChar s1 c :+: EIntA (                      AInt.eq i )) :=: EStrLen s2
  (EStrLen s2 :-: EIntA î  ) :=: EStrFirstIndexOfChar s1 c -> abstract x b $ (EStrFirstIndexOfChar s1 c :+: EIntA                                î  ) :=: EStrLen s2
  -----------------------------------------------------------
  (EStrFirstIndexOfChar (EVar s1) (EChar  c _) :+: EInt  i _) :=: EStrLen (EVar s2) | x == s1, x == s2 -> Just $ EStrA $ strWithFirstIndexOfCharRev (AChar.eq c) (AInt.eq i)
  (EStrFirstIndexOfChar (EVar s1) (ECharA ĉ  ) :+: EInt  i _) :=: EStrLen (EVar s2) | x == s1, x == s2 -> Just $ EStrA $ strWithFirstIndexOfCharRev ĉ (AInt.eq i)
  (EStrFirstIndexOfChar (EVar s1) (EChar  c _) :+: EIntA î  ) :=: EStrLen (EVar s2) | x == s1, x == s2 -> Just $ EStrA $ strWithFirstIndexOfCharRev (AChar.eq c) î
  (EStrFirstIndexOfChar (EVar s1) (ECharA ĉ  ) :+: EIntA î  ) :=: EStrLen (EVar s2) | x == s1, x == s2 -> Just $ EStrA $ strWithFirstIndexOfCharRev ĉ î
  -----------------------------------------------------------
  -- TODO: generalize these special cases
  (EStrFirstIndexOfChar (EVar s1) (EChar c1 _) :+: EIntA î) :=: EStrFirstIndexOfChar (EVar s2) (EChar c2 _)
    | x == s1, x == s2, c1 /= c2, î == î ∧ AInt.ge 0
    -> Just $ EStrA $ star (lit (AChar.ne c1 ∧ AChar.ne c2)) <> opt ((lit (AChar.eq c2) <> star (lit (AChar.ne c1))) ∨ (lit (AChar.eq c1) <> star anyChar))
  (EStrFirstIndexOfChar (EVar s1) (EChar c1 _) :-: EIntA î) :=: EStrFirstIndexOfChar (EVar s2) (EChar c2 _)
    | x == s1, x == s2, c1 /= c2, î == î ∧ AInt.le 1
    -> Just $ EStrA $ star (lit (AChar.ne c1 ∧ AChar.ne c2)) <> opt ((lit (AChar.eq c2) <> star (lit (AChar.ne c1))) ∨ ((lit (AChar.eq c1)) <> star anyChar))
  -----------------------------------------------------------
  e1 :=: e2 | x `freeIn` e1, x `freeIn` e2    -> Nothing
  e1 :≠: e2 | x `freeIn` e1, x `freeIn` e2    -> Nothing
  e1 :<: e2 | x `freeIn` e1, x `freeIn` e2    -> Nothing
  e1 :≤: e2 | x `freeIn` e1, x `freeIn` e2    -> Nothing
  e1 :>: e2 | x `freeIn` e1, x `freeIn` e2    -> Nothing
  e1 :≥: e2 | x `freeIn` e1, x `freeIn` e2    -> Nothing
  -- NOTE: below here, x occurs only on the LHS (possibly more than once)
  -----------------------------------------------------------
  EVar _ :=: EStrComp (EStr  s _)             -> Just $ EStrA (neg $ AString.eq $ Text.unpack s)
  EVar _ :=: EStrComp (EStrA s )              -> Just $ EStrA (neg s)
  EVar _ :=: e                                -> Just e
  -----------------------------------------------------------
  EVar _ :≠: EBool c pv                       -> Just $ EBool (not c) pv
  EVar _ :≠: e | b == TBool                   -> Just $ normExpr $ ENot e
  -----------------------------------------------------------
  EVar _ :≠: EInt c _                         -> Just $ EIntA (AInt.ne c)
  EVar _ :≠: EIntA c                          -> Just $ EIntA (neg c)
  EVar _ :<: EInt c _                         -> Just $ EIntA (AInt.lt c)
  EVar _ :≤: EInt c _                         -> Just $ EIntA (AInt.le c)
  EVar _ :>: EInt c _                         -> Just $ EIntA (AInt.gt c)
  EVar _ :≥: EInt c _                         -> Just $ EIntA (AInt.ge c)
  -----------------------------------------------------------
  EVar _ :<: e                                -> Just $ normExpr $ e :-: EIntA (AInt.ge 1)
  EVar _ :≤: e                                -> Just $ normExpr $ e :-: EIntA (AInt.ge 0)
  EVar _ :>: e                                -> Just $ normExpr $ e :+: EIntA (AInt.ge 1)
  EVar _ :≥: e                                -> Just $ normExpr $ e :+: EIntA (AInt.ge 0)
  -----------------------------------------------------------
  EVar _ :≠: EChar c _                        -> Just $ ECharA (AChar.ne c)
  EVar _ :≠: ECharA c                         -> Just $ ECharA (neg c)
  -----------------------------------------------------------
  EVar _ :≠: EStr s _                         -> Just $ EStrA (neg $ AString.eq $ Text.unpack s)
  EVar _ :≠: EStrA s                          -> Just $ EStrA (neg s)  
  EVar _ :≠: e | b == TString                 -> Just $ EStrComp e
  -----------------------------------------------------------
  e :∈: EStrA s                               -> abstract x b $ e :=: EStrA s
  e :∈: EReg ere                              -> abstract x b $ e :=: (EStrA $ AString.fromRegex $ Regex.POSIX.ERE.toRegex ere)
  e :∉: EReg ere                              -> abstract x b $ e :≠: (EStrA $ AString.fromRegex $ Regex.POSIX.ERE.toRegex ere)
  -----------------------------------------------------------
  (EVar _ :+: EInt  c pv) :=: e               -> Just $ normExpr $ e :-: EInt  c pv
  (EVar _ :+: EIntA c   ) :=: e               -> Just $ normExpr $ e :-: EIntA c
  (EVar _ :-: EInt  c pv) :=: e               -> Just $ normExpr $ e :+: EInt  c pv
  (EVar _ :-: EIntA c   ) :=: e               -> Just $ normExpr $ e :+: EIntA c
  -----------------------------------------------------------
  EStrLen (EVar _) :=: EInt  n _              -> Just $ EStrA $ strOfLen (AInt.eq n)
  EStrLen (EVar _) :=: EIntA n̂                -> Just $ EStrA $ strOfLen n̂
  EStrLen (EVar _) :<: EInt  n _              -> Just $ EStrA $ strOfLen (AInt.lt n)
  EStrLen (EVar _) :≤: EInt  n _              -> Just $ EStrA $ strOfLen (AInt.le n)
  EStrLen (EVar _) :>: EInt  n _              -> Just $ EStrA $ strOfLen (AInt.gt n)
  EStrLen (EVar _) :≥: EInt  n _              -> Just $ EStrA $ strOfLen (AInt.ge n)
  -----------------------------------------------------------
  EStrLen (EVar _) :≠: EInt  n _              -> Just $ EStrA $ strNotOfLen (AInt.eq n)
  EStrLen (EVar _) :≠: EIntA n̂                -> Just $ EStrA $ strNotOfLen n̂
  -----------------------------------------------------------
  EStrAt (EVar _) (EInt  i _) :=: EChar  c _ -> Just $ EStrA $ strWithCharAt (AInt.eq i) (AChar.eq c)
  EStrAt (EVar _) (EInt  i _) :=: ECharA ĉ   -> Just $ EStrA $ strWithCharAt (AInt.eq i) ĉ
  EStrAt (EVar _) (EIntA î  ) :=: EChar  c _ -> Just $ EStrA $ strWithCharAt î (AChar.eq c)
  EStrAt (EVar _) (EIntA î  ) :=: ECharA ĉ   -> Just $ EStrA $ strWithCharAt î ĉ
  -----------------------------------------------------------
  EStrAt (EVar _) (EInt  i _) :≠: EChar  c _ -> Just $ EStrA $ strWithoutCharAt (AInt.eq i) (AChar.eq c)
  EStrAt (EVar _) (EInt  i _) :≠: ECharA ĉ   -> Just $ EStrA $ strWithoutCharAt (AInt.eq i) ĉ
  EStrAt (EVar _) (EIntA î  ) :≠: EChar  c _ -> Just $ EStrA $ strWithoutCharAt î (AChar.eq c)
  EStrAt (EVar _) (EIntA î  ) :≠: ECharA ĉ   -> Just $ EStrA $ strWithoutCharAt î ĉ  
  -----------------------------------------------------------
  EStrAt (EVar s1) (EStrLen (EVar s2) :-: EInt  i _) :=: EChar  c _ | x == s1, x == s2 -> Just $ EStrA $ strWithCharAtRev (AInt.eq i) (AChar.eq c)
  EStrAt (EVar s1) (EStrLen (EVar s2) :-: EInt  i _) :=: ECharA ĉ   | x == s1, x == s2 -> Just $ EStrA $ strWithCharAtRev (AInt.eq i) ĉ
  EStrAt (EVar s1) (EStrLen (EVar s2) :-: EIntA î  ) :=: EChar  c _ | x == s1, x == s2 -> Just $ EStrA $ strWithCharAtRev î (AChar.eq c)
  EStrAt (EVar s1) (EStrLen (EVar s2) :-: EIntA î  ) :=: ECharA ĉ   | x == s1, x == s2 -> Just $ EStrA $ strWithCharAtRev î ĉ
  EStrAt (EVar s1) (EStrLen (EVar s2) :+: EIntA TOP) :=: EChar  c _ | x == s1, x == s2 -> Just $ EStrA $ strWithCharAtRev TOP (AChar.eq c)
  EStrAt (EVar s1) (EStrLen (EVar s2) :+: EIntA TOP) :=: ECharA ĉ   | x == s1, x == s2 -> Just $ EStrA $ strWithCharAtRev TOP ĉ
  EStrAt (EVar s1) (EStrLen (EVar s2) :+: EIntA î  ) :=: c          | x == s1, x == s2 -> abstract x b $ EStrAt (EVar s1) (EStrLen (EVar s2) :-: EIntA (AInt.sub (AInt.eq 0) î)) :=: c
  -----------------------------------------------------------
  EStrAt (EVar s1) (EStrLen (EVar s2) :-: EInt  i _) :≠: EChar  c _ | x == s1, x == s2 -> Just $ EStrA $ strWithoutCharAtRev (AInt.eq i) (AChar.eq c)
  EStrAt (EVar s1) (EStrLen (EVar s2) :-: EInt  i _) :≠: ECharA ĉ   | x == s1, x == s2 -> Just $ EStrA $ strWithoutCharAtRev (AInt.eq i) ĉ
  EStrAt (EVar s1) (EStrLen (EVar s2) :-: EIntA î  ) :≠: EChar  c _ | x == s1, x == s2 -> Just $ EStrA $ strWithoutCharAtRev î (AChar.eq c)  
  EStrAt (EVar s1) (EStrLen (EVar s2) :-: EIntA î  ) :≠: ECharA ĉ   | x == s1, x == s2 -> Just $ EStrA $ strWithoutCharAtRev î ĉ
  EStrAt (EVar s1) (EStrLen (EVar s2) :+: EIntA TOP) :≠: EChar  c _ | x == s1, x == s2 -> Just $ EStrA $ strWithoutCharAtRev TOP (AChar.eq c)
  EStrAt (EVar s1) (EStrLen (EVar s2) :+: EIntA TOP) :≠: ECharA ĉ   | x == s1, x == s2 -> Just $ EStrA $ strWithoutCharAtRev TOP ĉ
  -----------------------------------------------------------
  EStrSub (EVar _) (EInt  i _) (EInt  j _) :=: EStr  t _ -> Just $ EStrA $ strWithSubstr (AInt.eq i) (AInt.eq j) (AString.eq $ Text.unpack t)
  EStrSub (EVar _) (EIntA î  ) (EInt  j _) :=: EStr  t _ -> Just $ EStrA $ strWithSubstr î (AInt.eq j) (AString.eq $ Text.unpack t)
  EStrSub (EVar _) (EInt  i _) (EIntA ĵ  ) :=: EStr  t _ -> Just $ EStrA $ strWithSubstr (AInt.eq i) ĵ (AString.eq $ Text.unpack t)
  EStrSub (EVar _) (EIntA î  ) (EIntA ĵ  ) :=: EStr  t _ -> Just $ EStrA $ strWithSubstr î ĵ (AString.eq $ Text.unpack t)
  EStrSub (EVar _) (EInt  i _) (EInt  j _) :=: EStrA t̂   -> Just $ EStrA $ strWithSubstr (AInt.eq i) (AInt.eq j) t̂
  EStrSub (EVar _) (EIntA î  ) (EInt  j _) :=: EStrA t̂   -> Just $ EStrA $ strWithSubstr î (AInt.eq j) t̂
  EStrSub (EVar _) (EInt  i _) (EIntA ĵ  ) :=: EStrA t̂   -> Just $ EStrA $ strWithSubstr (AInt.eq i) ĵ t̂
  EStrSub (EVar _) (EIntA î  ) (EIntA ĵ  ) :=: EStrA t̂   -> Just $ EStrA $ strWithSubstr î ĵ t̂
  -----------------------------------------------------------
  EStrSub (EVar _) (EInt  i _) (EInt  j _) :≠: EStr  t _ -> Just $ EStrA $ strWithoutSubstr (AInt.eq i) (AInt.eq j) (AString.eq $ Text.unpack t)
  EStrSub (EVar _) (EIntA î  ) (EInt  j _) :≠: EStr  t _ -> Just $ EStrA $ strWithoutSubstr î (AInt.eq j) (AString.eq $ Text.unpack t)
  EStrSub (EVar _) (EInt  i _) (EIntA ĵ  ) :≠: EStr  t _ -> Just $ EStrA $ strWithoutSubstr (AInt.eq i) ĵ (AString.eq $ Text.unpack t)
  EStrSub (EVar _) (EIntA î  ) (EIntA ĵ  ) :≠: EStr  t _ -> Just $ EStrA $ strWithoutSubstr î ĵ (AString.eq $ Text.unpack t)
  EStrSub (EVar _) (EInt  i _) (EInt  j _) :≠: EStrA t̂   -> Just $ EStrA $ strWithoutSubstr (AInt.eq i) (AInt.eq j) t̂
  EStrSub (EVar _) (EIntA î  ) (EInt  j _) :≠: EStrA t̂   -> Just $ EStrA $ strWithoutSubstr î (AInt.eq j) t̂
  EStrSub (EVar _) (EInt  i _) (EIntA ĵ  ) :≠: EStrA t̂   -> Just $ EStrA $ strWithoutSubstr (AInt.eq i) ĵ t̂
  EStrSub (EVar _) (EIntA î  ) (EIntA ĵ  ) :≠: EStrA t̂   -> Just $ EStrA $ strWithoutSubstr î ĵ t̂
  -----------------------------------------------------------
  EStrFirstIndexOfChar (EVar _) (EChar  c _) :=: EInt  i _ -> Just $ EStrA $ strWithFirstIndexOfChar (AChar.eq c) (AInt.eq i)
  EStrFirstIndexOfChar (EVar _) (ECharA ĉ  ) :=: EInt  i _ -> Just $ EStrA $ strWithFirstIndexOfChar ĉ (AInt.eq i)
  EStrFirstIndexOfChar (EVar _) (EChar  c _) :=: EIntA î   -> Just $ EStrA $ strWithFirstIndexOfChar (AChar.eq c) î
  EStrFirstIndexOfChar (EVar _) (ECharA ĉ  ) :=: EIntA î   -> Just $ EStrA $ strWithFirstIndexOfChar ĉ î
  -----------------------------------------------------------
  EStrSub (EVar s1) (EInt i _) (EStrFirstIndexOfChar (EVar s2) (EChar c _) :-: EInt j _) :=: EStrA t̂ 
    | x == s1, x == s2, i >= 0, j >= 0 -> Just $ EStrA $ rep c̄ i <> (t̂ ∧ star c̄) <> rep c̄ (j-1) <> lit ĉ <> star anyChar
    where 
      ĉ = AChar.eq c
      c̄ = lit (neg ĉ)
  -----------------------------------------------------------
  EStrSub (EVar s1) (EStrFirstIndexOfChar (EVar s2) (EChar c _) :+: EInt i _) (EStrLen (EVar s3) :-: EInt j _) :=: EStr  t _ | x == s1, x == s2, x == s3 -> Just $ EStrA $ strWithSubstrFromFirstIndexOfCharToEnd (AChar.eq c) i j (AString.eq $ Text.unpack t)
  EStrSub (EVar s1) (EStrFirstIndexOfChar (EVar s2) (EChar c _) :+: EInt i _) (EStrLen (EVar s3) :-: EInt j _) :=: EStrA t̂   | x == s1, x == s2, x == s3 -> Just $ EStrA $ strWithSubstrFromFirstIndexOfCharToEnd (AChar.eq c) i j t̂
  -----------------------------------------------------------
  EStrIndexOf (EVar _) (EStr  (   Text.unpack ->      [c]) _) (EInt 0 _) :=: EInt  i _ -> Just $ EStrA $ strWithFirstIndexOfChar (AChar.eq c) (AInt.eq i)
  EStrIndexOf (EVar _) (EStrA (AString.toChar -> Just  ĉ )  ) (EInt 0 _) :=: EInt  i _ -> Just $ EStrA $ strWithFirstIndexOfChar ĉ (AInt.eq i)
  EStrIndexOf (EVar _) (EStr  (   Text.unpack ->      [c]) _) (EInt 0 _) :=: EIntA î   -> Just $ EStrA $ strWithFirstIndexOfChar (AChar.eq c) î
  EStrIndexOf (EVar _) (EStrA (AString.toChar -> Just  ĉ )  ) (EInt 0 _) :=: EIntA î   -> Just $ EStrA $ strWithFirstIndexOfChar ĉ î
  -----------------------------------------------------------
  EStrIndexOf (EVar s1) (EStr (Text.unpack -> [c1]) _) (EStrIndexOf (EVar s2) (EStr (Text.unpack -> [c2]) _) (EInt 0 _) :+: EInt 1 _) :=: EIntA k̂
    | x == s1, s1 == s2 -> Just $ EStrA $ strWithFirstIndexOfCharFollowedByFirstIndexOfChar (AChar.eq c2) (AChar.eq c1) k̂
  -----------------------------------------------------------
  EStrAt (EVar s1) (EStrIndexOf (EVar s2) (EStr t1 _) (EInt i _) :+: EInt n _) :=: EChar c2 _
    | x == s1, x == s2
    , [c1] <- Text.unpack t1
    , let ĉ1 = AChar.eq c1, let c̄1 = AChar.ne c1
    , let ĉ2 = AChar.eq c2
    -> Just $ EStrA $ rep anyChar i <> star (lit c̄1) <> lit ĉ1 <> rep anyChar (n - 1) <> lit ĉ2 <> star anyChar
  -----------------------------------------------------------
  EStrContains (EVar _) (EStr s _) :=: EBool doesContain _
    | doesContain -> Just $ EStrA $ star anyChar <> AString.eq (Text.unpack s) <> star anyChar
    | otherwise   -> Just $ EStrComp $ EStrA $ star anyChar <> AString.eq (Text.unpack s) <> star anyChar
  EStrContains (EVar _) (EStrA s ) :=: EBool doesContain _
    | doesContain -> Just $ EStrA $ star anyChar <> s <> star anyChar
    | otherwise   -> Just $ EStrComp $ EStrA $ star anyChar <> s <> star anyChar
  -----------------------------------------------------------
  _                                           -> Nothing

strOfLen :: AInt -> AString
strOfLen (meet (AInt.ge 0) -> n̂)
  | isBot n̂ = bot
  | otherwise = joins $ AInt.intervals n̂ >>= \case
      AInt.In (Fin a) (Fin b) -> [rep anyChar n | n <- [a..b]]
      AInt.In (Fin a) PosInf  -> [rep anyChar a <> star anyChar]
      _                       -> impossible

strNotOfLen :: AInt -> AString
strNotOfLen (meet (AInt.ge 0) -> n̂)
  | isBot n̂ = top
  | otherwise = meets $ AInt.intervals n̂ >>= \case
      AInt.In (Fin a) (Fin b) -> [ joins $ [rep anyChar i | i <- [0..n-1]] 
                                        ++ [rep anyChar (n + 1) <> star anyChar]
                                 | n <- [a..b] ]
      AInt.In (Fin a) PosInf  -> [ joins $ [rep anyChar i | i <- [0..a-1]] ]
      _                       -> impossible

strWithCharAt :: AInt -> AChar -> AString
strWithCharAt (meet (AInt.ge 0) -> î) ĉ
  | isBot ĉ = strOfLen î  -- TODO: should this be min î ?
  | otherwise = joins $ AInt.intervals î >>= \case
      AInt.In (Fin a) (Fin b) -> [rep anyChar i <> lit ĉ <> star anyChar | i <- [a..b]]
      AInt.In (Fin a) PosInf  -> [rep anyChar a <> star anyChar <> lit ĉ <> star anyChar]
      _                       -> impossible

strWithoutCharAt :: AInt -> AChar -> AString
strWithoutCharAt (meet (AInt.ge 0) -> î) ĉ
  | isBot ĉ = strOfLen (AInt.geA î)
  | otherwise = meets $ AInt.intervals î >>= \case
      AInt.In (Fin a) (Fin b) -> [ joins $ [rep anyChar n | n <- [0..i]]
                                        ++ [rep anyChar i <> c̄ <> star anyChar] 
                                 | i <- [a..b] ]
      AInt.In (Fin a) PosInf  -> [rep anyChar a <> star c̄]
      _                       -> impossible
 where
  c̄ = lit (neg ĉ)

strWithCharAtRev :: AInt -> AChar -> AString
strWithCharAtRev (meet (AInt.ge 1) -> î) ĉ
  | isBot ĉ = bot
  | otherwise = joins $ AInt.intervals î >>= \case
      AInt.In (Fin a) (Fin b) -> [star anyChar <> lit ĉ <> rep2 anyChar (a - 1) (b - 1)]
      AInt.In (Fin a) PosInf  -> [star anyChar <> lit ĉ <> rep anyChar (a - 1) <> star anyChar]
      _                       -> impossible

strWithoutCharAtRev :: AInt -> AChar -> AString
strWithoutCharAtRev (meet (AInt.ge 1) -> î) ĉ
  | isBot ĉ = top
  | otherwise = meets $ AInt.intervals î >>= \case
      AInt.In (Fin a) (Fin b) -> [go a b]
      AInt.In (Fin a) PosInf  -> [(star c̄ <> rep anyChar (a - 1)) ∨ rep2 anyChar 0 (a - 1)]
      _                       -> impossible
 where
  c̄ = lit (neg ĉ)
  go a b
    | a >  b    = impossible
    | a == b    = (star anyChar <> c̄ <> rep anyChar (a - 1)) ∨ rep2 anyChar 0 (a - 1)    
    | otherwise = opt $ go a (b - 1) <> anyChar    

strWithSubstr :: AInt -> AInt -> AString -> AString
strWithSubstr (meet (AInt.ge 0) -> î) (meet (AInt.geA î) -> ĵ) t̂
  | isBot î = bot
  | isBot ĵ = strOfLen î
  | otherwise = joins $ AInt.intervals î >>= \case
      AInt.In (Fin a) (Fin b) -> AInt.intervals ĵ >>= \case
        AInt.In (Fin c) (Fin d) -> [str  i j | i <- [a..b], j <- [c..d]]
        AInt.In (Fin c) PosInf  -> [str' i c | i <- [a..b]]
        _                       -> impossible
      AInt.In (Fin a) PosInf  -> AInt.intervals ĵ >>= \case
        AInt.In (Fin c) (Fin d) -> [str  a j | j <- [c..d], a <= j]
        AInt.In (Fin c) PosInf  -> [str' a c]
        _                       -> impossible
      _                       -> impossible
 where
  str  i j = rep anyChar i <>  (rep anyChar (j - i + 1)                  ∧ t̂) <> star anyChar
  str' i j = rep anyChar i <> ((rep anyChar (j - i + 1) <> star anyChar) ∧ t̂) <> star anyChar

strWithoutSubstr :: AInt -> AInt -> AString -> AString
strWithoutSubstr î ĵ t̂ = neg $ strWithSubstr î ĵ t̂

strWithFirstIndexOfChar :: AChar -> AInt -> AString
strWithFirstIndexOfChar ĉ î
  | isBot ĉ = strOfLen î -- TODO: should this be min î ?
  | Just m <- AInt.minimum î, m < Fin 0 
      = (star c̄) ∨ strWithFirstIndexOfChar ĉ (î ∧ AInt.ge 0)
  | otherwise = joins $ AInt.intervals î >>= \case
      AInt.In (Fin a) (Fin b) -> [rep c̄ i <> lit ĉ <> star anyChar | i <- [a..b]]
      AInt.In (Fin a) PosInf  -> [rep c̄ a <> star c̄ <> lit ĉ <> star anyChar]
      _                       -> impossible
 where
  c̄ = lit (neg ĉ)

strWithFirstIndexOfCharRev :: AChar -> AInt -> AString
strWithFirstIndexOfCharRev ĉ î
  | isBot ĉ = undefined -- TODO
  | Just m <- AInt.minimum î, m < Fin 1
      = (star c̄) ∨ strWithFirstIndexOfCharRev ĉ (î ∧ AInt.ge 1)
  | otherwise = joins $ AInt.intervals î >>= \case
      AInt.In (Fin a) (Fin b) -> [star c̄ <> lit ĉ <> rep2 anyChar (a - 1) (b - 1)]
      AInt.In (Fin a) PosInf  -> [star c̄ <> lit ĉ <> rep anyChar (a - 1) <> star anyChar]
      _                       -> impossible
 where
  c̄ = lit (neg ĉ)

strWithFirstIndexOfCharFollowedByFirstIndexOfChar :: AChar -> AChar -> AInt -> AString
strWithFirstIndexOfCharFollowedByFirstIndexOfChar ĉ1 ĉ2 (meet (AInt.ge 1) -> î) -- TODO
  | not (isBot (ĉ1 ∧ ĉ2)) = undefined -- TODO
  | otherwise = joins $ AInt.intervals î >>= \case
      AInt.In (Fin a) (Fin b) -> [star c̄12 <> lit ĉ1 <> rep2 c̄2 a b <> lit ĉ2 <> star anyChar]
      AInt.In (Fin a) PosInf  -> [star c̄12 <> lit ĉ1 <> rep c̄2 (a - 1) <> star c̄2 <> lit ĉ2 <> star anyChar]
      _                       -> impossible
 where
  c̄12 = lit (neg ĉ1 ∧ neg ĉ2)
  c̄2  = lit (neg ĉ2)

strWithSubstrFromFirstIndexOfCharToEnd :: AChar -> Integer -> Integer -> AString -> AString
strWithSubstrFromFirstIndexOfCharToEnd ĉ i j t̂
  | i < 1 || j < 1 = undefined -- TODO
  | otherwise = star c̄ <> lit ĉ <> rep anyChar (i-1) <> t̂ <> rep anyChar (j-1)
 where
  c̄ = lit (neg ĉ)

-------------------------------------------------------------------------------

concretizeVar :: Name -> Base -> AValue -> Pan Pred
concretizeVar x b v = logAndReturn $ case (b,v) of
  (TUnit  , AUnit   a) -> concretizeUnit   x a
  (TBool  , ABool   a) -> concretizeBool   x a
  (TInt   , AInt    a) -> concretizeInt    x a
  (TChar  , AChar   a) -> concretizeChar   x a
  (TString, AString a) -> concretizeString x a
  _ -> panic $ "concretizeVar:" <+> pretty x <+> pretty b <+> pretty v      
 where
  logAndReturn p = do
    logMessage $ "⟦" <> pretty v <> "⟧↓" <> pretty x <+> "≐" <+> pretty p
    return p

concretizeUnit :: Name -> AUnit -> Pred
concretizeUnit x a = case a of
  AUnit.Unit   -> PRel $ EVar x :=: EUnit NoPV
  AUnit.Bottom -> PFalse

concretizeBool :: Name -> ABool -> Pred
concretizeBool x a = case ABool.value a of
  Just b  -> PRel $ EVar x :=: EBool b NoPV
  Nothing -> if isTop a then PTrue else PFalse

concretizeInt :: Name -> AInt -> Pred
concretizeInt x a = case AInt.intervals a of
  []                                        -> PFalse  
  [NegInf :..: PosInf]                      -> PTrue
  [NegInf :..: Fin n ]                      -> mk (:≤:) n
  [Fin m  :..: PosInf]                      -> mk (:>:) m
  [Fin m  :..: Fin n ] | m == n             -> mk (:=:) m
                       | otherwise          -> mk (:≥:) m ∧ mk (:≤:) n
  [NegInf :..: Fin m, Fin n :..: PosInf]
                               | n - m == 2 -> mk (:≠:) (m + 1)
                               | otherwise  -> mk (:≤:) m ∨ mk (:≥:) n
  (Fin m  :..: _) : (last -> _ :..: Fin n ) -> mk (:≥:) m ∧ mk (:≤:) n ∧ mkHoles
  (NegInf :..: _) : (last -> _ :..: Fin n ) -> mk (:≤:) n ∧ mkHoles
  (Fin m  :..: _) : (last -> _ :..: PosInf) -> mk (:≥:) m ∧ mkHoles
  (NegInf :..: _) : (last -> _ :..: PosInf) -> mkHoles
  _                                         -> impossible
 where
  mk op n = PRel $ op (EVar x) (EInt (fromIntegral n) NoPV)
  mkHoles = meets $ map (mk (:≠:)) $ AInt.holes $ AInt.intervals a

-- TODO: move to AInt module
pattern (:..:) :: Inf Integer -> Inf Integer -> AInt.Interval
pattern a :..: b = AInt.In a b

concretizeChar :: Name -> AChar -> Pred
concretizeChar x ĉ
  | [c] <- AChar.values (neg ĉ) = PRel $ EVar x :≠: EChar c NoPV
  | isBot ĉ   = PFalse
  | isTop ĉ   = PTrue
  | otherwise = joins $ [PRel $ EVar x :=: EChar c NoPV | c <- AChar.values ĉ]

concretizeString :: Name -> AString -> Pred
concretizeString x a = case AString.toRegex a of
  Regex.Zero -> PFalse
  Regex.One  -> PRel $ EVar x :=: EStr "" NoPV
  Regex.All  -> PTrue
  r -> case Regex.POSIX.ERE.fromRegex r of
    Just ere -> PRel $ EVar x :∈: EReg ere
    Nothing  -> panic $ "cannot convert Regex to ERE:" <+> pretty r
