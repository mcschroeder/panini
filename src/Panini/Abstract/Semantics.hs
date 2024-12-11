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
import Panini.Syntax
import Prelude
import Regex qualified as Regex
import Regex.Type (prettyRegex)
import Regex.Inclusion qualified as Regex
import Regex.POSIX.ERE qualified

--import Debug.Trace
trace :: String -> a -> a
trace _ = id

-------------------------------------------------------------------------------

-- | Normalize an abstract expression by (partial) evaluation; see 'normRel'.
normExprA :: ExprA -> ExprA
-- normExprA = \case
normExprA e0 = trace ("normExprA " ++ showPretty e0) $ case e0 of
  -----------------------------------------------------------
  ESol x b r -> case normRelA r of
    Left True                                 -> EAbs (topValue b)
    Left False                                -> EAbs (botValue b)
    Right r' | r' /= r                        -> normExprA $ ESol x b r'
             | Just e <- abstract x b r       -> e
    _                                         -> e0
  -----------------------------------------------------------
--EFun _ es | any hasBot (universeBi =<< es)  -> botExpr ??
  -----------------------------------------------------------
  ENot (EBoolA a)                             -> normExprA $ EBoolA (neg a)
  ENot (ENot e)                               -> normExprA $ e
  -----------------------------------------------------------
  EIntA BOT :+: _                             -> EIntA BOT
  _         :+: EIntA BOT                     -> EIntA BOT
  EIntA a   :+: EIntA b                       -> normExprA $ EIntA $ AInt.add a b
  a         :+: EIntA AInt0                   -> normExprA $ a
  a         :+: EIntA b    | b `AInt.isLe` 0  -> normExprA $ a :-: EIntA (AInt0 `AInt.sub` b)
  a         :+: b          | a > b            -> normExprA $ b :+: a
  (a :+: b) :+: c          | (b ⏚), (c ⏚)    -> normExprA $ a :+: (normExprA $ b :+: c)
  (a :-: b) :+: c          | (b ⏚), (c ⏚)    -> normExprA $ a :-: (normExprA $ b :-: c)
  -----------------------------------------------------------
  EIntA BOT :-: _                             -> EIntA BOT
  _         :-: EIntA BOT                     -> EIntA BOT
  EIntA a   :-: EIntA b                       -> normExprA $ EIntA $ AInt.sub a b
  a         :-: EIntA AInt0                   -> normExprA $ a
  a         :-: EIntA b    | b `AInt.isLe` 0  -> normExprA $ a :+: EIntA (AInt0 `AInt.sub` b)
  (a :-: b) :-: c          | (b ⏚), (c ⏚)    -> normExprA $ a :-: (normExprA $ b :+: c)
  (a :+: b) :-: c          | (b ⏚), (c ⏚)    -> normExprA $ a :+: (normExprA $ b :-: c)
  (a :+: b) :-: c          | (a ⏚), (c ⏚)    -> normExprA $ b :+: (normExprA $ a :-: c)
  -----------------------------------------------------------
  EMod (EIntA â) (EIntA b̂) | [a] <- AInt.values â, [b] <- AInt.values b̂ -> EIntA $ AInt.eq (a `mod` b)
  -----------------------------------------------------------
  EStrLen (EStrA a) | isTop a                 -> EIntA (AInt.ge 0)
  EStrLen (EStrA a) | Just n <- strLen1 a     -> EIntA (AInt.eq n)
  -- NOTE: We don't have any efficient way to compute nor represent, in general,
  -- the precise lengths of all strings contained in an abstract string.
  -----------------------------------------------------------
  EStrAt (EStrA (AString1 s)) (EIntA i)                 -> normExprA $ ECharA $ charAt s i
  -----------------------------------------------------------
  EStrAt (EVar s1) (EStrLen (EVar s2)) | s1 == s2 -> ECharA bot
  -----------------------------------------------------------
  EStrSub (EStrA (AString1 s)) (EIntA i) (EIntA j) -> normExprA $ EStrA $ strSub s i j
  EStrSub s1 (EIntA AInt0) (EStrLen s2 :-: EIntA AInt1) | s1 == s2 -> normExprA s1
  -----------------------------------------------------------
  -- NOTE: We want to defer resolution of EStrComp as long as possible, 
  -- in order to exploit opportunities for double-negation cancellation!   
  -- EStrComp (EStr s _)                      -> normExprA $ EStrA (neg $ AString.eq $ Text.unpack s) 
  -- EStrComp (EStrA s)                       -> normExprA $ EStrA $ neg s
  EStrComp (EStrComp e)                       -> normExprA $ e  
  -----------------------------------------------------------
  EStrConc (EStrA a) (EStrA b)                -> normExprA $ EStrA (a <> b)  
  -----------------------------------------------------------
  EStrConc (EStrSub s1 (EIntA î1) (EIntA ĵ1)) (EStrSub s2 (EIntA î2) (EIntA ĵ2))
    | [i1] <- AInt.values î1, [j1] <- AInt.values ĵ1
    , [i2] <- AInt.values î2, [j2] <- AInt.values ĵ2
    , s1 == s2, i1 <= j1, j1 + 1 == i2, i2 <= j2
    -> normExprA $ EStrSub s1 (EIntA î1) (EIntA ĵ2)
  -----------------------------------------------------------
  EStrStar (EStrA s)                          -> normExprA $ EStrA $ star s
  -----------------------------------------------------------
  -- TODO
  EStrContains (EStrA (MkAString r1)) (EStrA (MkAString r2)) -> EBoolA $ ABool.eq $ r2 `Regex.isIncludedBy` r1
  -----------------------------------------------------------
  e | e' <- descend normExprA e, e' /= e       -> normExprA e'
    | otherwise                               -> e

-- | Normalize an abstract relation by (partial) evaluation; see 'normRel'.
normRelA :: RelA -> Either Bool RelA
-- normRelA = \case
normRelA r0 = trace ("normRelA " ++ showPretty r0) $ case r0 of
  -----------------------------------------------------------
  EAbs  a   :=: EAbs   b   | Just m <- a ∧? b -> Left (not $ hasBot m)
  _         :=: EAbs b     | hasBot b         -> Left False
  a         :=: b          | a == b           -> Left True
  -----------------------------------------------------------
  EAbs  a   :≠: EAbs   b   | Just m <- a ∧? b -> Left (hasBot m)
  a         :≠: b          | a == b           -> Left False
  -----------------------------------------------------------
  a         :<: b          | a == b           -> Left False
  a         :≤: b          | a == b           -> Left True
  a         :>: b          | a == b           -> Left False
  a         :≥: b          | a == b           -> Left True
  -----------------------------------------------------------
  -- NOTE: ">" is the structural ordering on 'Expr'; after 
  -- this block, the "smaller" expression will be on the LHS,
  -- with variables < functions < constants
  a :=: b | a > b                             -> normRelA $ b :=: a
  a :≠: b | a > b                             -> normRelA $ b :≠: a
  a :<: b | a > b                             -> normRelA $ b :>: a
  a :≤: b | a > b                             -> normRelA $ b :≥: a
  a :>: b | a > b                             -> normRelA $ b :<: a
  a :≥: b | a > b                             -> normRelA $ b :≤: a
  -----------------------------------------------------------
  ENot a :=: ENot b                           -> normRelA $ a :=: b
  ENot a :≠: ENot b                           -> normRelA $ a :≠: b
  ENot a :=: b                                -> normRelA $ a :≠: b
  ENot a :≠: b                                -> normRelA $ a :=: b
  a      :=: ENot b                           -> normRelA $ a :≠: b
  a      :≠: ENot b                           -> normRelA $ a :=: b
  -----------------------------------------------------------
  a :=: (b :+: EIntA c) | a == b              -> Left (AInt.member 0 c)
  a :≠: (b :+: EIntA c) | a == b              -> Left (not $ AInt.member 0 c)
  a :<: (b :+: EIntA c) | a == b              -> Left (not $ isBot $ c ∧ AInt.gt 0)
  a :≤: (b :+: EIntA c) | a == b              -> Left (not $ isBot $ c ∧ AInt.ge 0)
  a :>: (b :+: EIntA c) | a == b              -> Left (not $ isBot $ c ∧ AInt.lt 0)
  a :≥: (b :+: EIntA c) | a == b              -> Left (not $ isBot $ c ∧ AInt.le 0)
  (b :+: EIntA c) :=: a | a == b              -> Left (AInt.member 0 c)
  (b :+: EIntA c) :>: a | a == b              -> Left (not $ isBot $ c ∧ AInt.gt 0)
  (b :+: EIntA c) :≥: a | a == b              -> Left (not $ isBot $ c ∧ AInt.ge 0)
  (b :+: EIntA c) :<: a | a == b              -> Left (not $ isBot $ c ∧ AInt.lt 0)
  (b :+: EIntA c) :≤: a | a == b              -> Left (not $ isBot $ c ∧ AInt.le 0)
  -----------------------------------------------------------
  a :=: (b :-: EIntA c) | a == b              -> Left (AInt.member 0 c)
  a :≠: (b :-: EIntA c) | a == b              -> Left (not $ AInt.member 0 c)
  a :<: (b :-: EIntA c) | a == b              -> Left (not $ isBot $ c ∧ AInt.lt 0)
  a :≤: (b :-: EIntA c) | a == b              -> Left (not $ isBot $ c ∧ AInt.le 0)
  a :>: (b :-: EIntA c) | a == b              -> Left (not $ isBot $ c ∧ AInt.gt 0)
  a :≥: (b :-: EIntA c) | a == b              -> Left (not $ isBot $ c ∧ AInt.ge 0)  
  (b :-: EIntA c) :=: a | a == b              -> Left (AInt.member 0 c)
  (b :-: EIntA c) :≠: a | a == b              -> Left (not $ AInt.member 0 c)
  (b :-: EIntA c) :>: a | a == b              -> Left (not $ isBot $ c ∧ AInt.lt 0)
  (b :-: EIntA c) :≥: a | a == b              -> Left (not $ isBot $ c ∧ AInt.le 0)
  (b :-: EIntA c) :<: a | a == b              -> Left (not $ isBot $ c ∧ AInt.gt 0)
  (b :-: EIntA c) :≤: a | a == b              -> Left (not $ isBot $ c ∧ AInt.ge 0)
  -----------------------------------------------------------
  a :<: (b :+: EIntA AInt1)                   -> normRelA $ a :≤: b
  a :≤: (b :-: EIntA AInt1)                   -> normRelA $ a :<: b
  a :>: (b :-: EIntA AInt1)                   -> normRelA $ a :≥: b
  a :≥: (b :+: EIntA AInt1)                   -> normRelA $ a :>: b
  -----------------------------------------------------------
  Rel op (a :-: EIntA b) (EIntA c) -> normRelA $ Rel op a (EIntA (AInt.add c b))
  -----------------------------------------------------------
  -- TODO: go over this again to make sure we don't loop (cf. above)
  Rel op (a :+: b) c           | (a ⏚), (c ⏚) -> normRelA $ Rel op b (normExprA $ c :-: a)
  Rel op (a :+: b) c           | (b ⏚), (c ⏚) -> normRelA $ Rel op a (normExprA $ c :-: b)
  Rel op (a :+: b) c@(_ :+: d) | (a ⏚), (d ⏚) -> normRelA $ Rel op b (normExprA $ c :-: a)
  Rel op (a :+: b) c@(_ :+: d) | (b ⏚), (d ⏚) -> normRelA $ Rel op a (normExprA $ c :-: b)
  Rel op (a :+: b) c@(d :+: _) | (a ⏚), (d ⏚) -> normRelA $ Rel op b (normExprA $ c :-: a)
  Rel op (a :+: b) c@(d :+: _) | (b ⏚), (d ⏚) -> normRelA $ Rel op a (normExprA $ c :-: b)
  Rel op (a :+: b) c@(_ :-: d) | (a ⏚), (d ⏚) -> normRelA $ Rel op b (normExprA $ c :-: a)
  Rel op (a :+: b) c@(_ :-: d) | (b ⏚), (d ⏚) -> normRelA $ Rel op a (normExprA $ c :-: b)
  Rel op (a :+: b) c@(d :-: _) | (a ⏚), (d ⏚) -> normRelA $ Rel op b (normExprA $ c :-: a)
  Rel op (a :+: b) c@(d :-: _) | (b ⏚), (d ⏚) -> normRelA $ Rel op a (normExprA $ c :-: b)
  Rel op (a :-: b) c           | (a ⏚), (c ⏚) -> normRelA $ Rel op b (normExprA $ a :-: c)
  Rel op (a :-: b) c           | (b ⏚), (c ⏚) -> normRelA $ Rel op a (normExprA $ c :+: b)
  Rel op (a :-: b) c@(_ :+: d) | (a ⏚), (d ⏚) -> normRelA $ Rel op b (normExprA $ a :-: c)
  Rel op (a :-: b) c@(_ :+: d) | (b ⏚), (d ⏚) -> normRelA $ Rel op a (normExprA $ c :+: b)
  Rel op (a :-: b) c@(d :+: _) | (a ⏚), (d ⏚) -> normRelA $ Rel op b (normExprA $ a :-: c)
  Rel op (a :-: b) c@(d :+: _) | (b ⏚), (d ⏚) -> normRelA $ Rel op a (normExprA $ c :+: b)
  Rel op (a :-: b) c@(_ :-: d) | (a ⏚), (d ⏚) -> normRelA $ Rel op b (normExprA $ a :-: c)
  Rel op (a :-: b) c@(_ :-: d) | (b ⏚), (d ⏚) -> normRelA $ Rel op a (normExprA $ c :+: b)
  Rel op (a :-: b) c@(d :-: _) | (a ⏚), (d ⏚) -> normRelA $ Rel op b (normExprA $ a :-: c)
  Rel op (a :-: b) c@(d :-: _) | (b ⏚), (d ⏚) -> normRelA $ Rel op a (normExprA $ c :+: b)
  -----------------------------------------------------------
  EMod (EIntA a) (EIntA b̂) :=: EIntA ĉ
    | [b] <- AInt.values b̂, [c] <- AInt.values ĉ
    , any (\x -> x `mod` b == c) $ take 100 $ AInt.values a -> Left True
  -----------------------------------------------------------
  EIntA î :=: ESol a1 TInt (EMod (EVar a2) (EIntA n̂) :=: EIntA m̂)
    | [n] <- AInt.values n̂, [m] <- AInt.values m̂
    , a1 == a2, n >= 0, m >= 0, AInt.ge 0 == î ∧ AInt.ge 0 -> Left True
  -----------------------------------------------------------
  EStrComp a :=: EStrComp b                   -> normRelA $ a :=: b
  EStrComp a :≠: EStrComp b                   -> normRelA $ a :≠: b
  EStrComp a :=: b                            -> normRelA $ a :≠: b
  EStrComp a :≠: b                            -> normRelA $ a :=: b
  a          :≠: EStrComp b                   -> normRelA $ a :=: b
  -----------------------------------------------------------
  (EStrFirstIndexOfChar s1 c :-: EIntA î) :=: EStrLen s2 -> normRelA $ (EStrFirstIndexOfChar s1 c :+: EIntA (AInt.sub (AInt.eq 0) î)) :=: EStrLen s2
  -----------------------------------------------------------
  EStrIndexOf s c i :≠: EIntA n̂ -> normRelA $ EStrIndexOf s c i :=: EIntA (neg n̂)
  EStrIndexOf (EVar s) (EStrA (AString.toChar -> Just ĉ)) (EIntA AInt0) :=: EIntA n̂ 
    -> normRelA $ EVar s :=: EStrA (strWithFirstIndexOfChar ĉ (n̂ ∧ AInt.ge (-1)))
  -----------------------------------------------------------
  i1 :+: EIntA n̂ :=: EStrIndexOf s c i2 
    | i1 == i2, let n̂' = n̂ ∧ AInt.ge 0, n̂' /= n̂ 
    -> normRelA $ i1 :+: EIntA n̂' :=: EStrIndexOf s c i2
  i1 :-: EIntA n̂ :=: EStrIndexOf s c i2 
    | i1 == i2, let n̂' = n̂ ∧ AInt.le 0, n̂' /= n̂ 
    -> normRelA $ i1 :-: EIntA n̂' :=: EStrIndexOf s c i2
  -----------------------------------------------------------
  EStrLen s1 :+: EIntA n̂ :=: EStrIndexOf s2 c i
    | s1 == s2, let n̂' = n̂ ∧ AInt.lt 0, n̂' /= n̂ 
    -> normRelA $ EStrLen s1 :+: EIntA n̂' :=: EStrIndexOf s2 c i
  -----------------------------------------------------------
  k :=: EStrIndexOf s t i | k == i 
    -> normRelA $ EStrSub s i (i :+: (EStrLen t :-: EIntA AInt1)) :=: t
  -----------------------------------------------------------
  EStrLen s1 :-: EIntA AInt1 :=: EStrIndexOf s2 (EStrA (AString1 t)) (EIntA î)
    | [i] <- AInt.values î
    , s1 == s2, [c] <- Text.unpack t
    , let ĉ = AChar.eq c, let c̄ = AChar.ne c    
    -> normRelA $ s1 :=: EStrA (rep anyChar i <> star (lit c̄) <> lit ĉ)
  -----------------------------------------------------------
  EStrSub s i1 i2 :=: EStrA t
    | i1 == i2, Just c <- AString.toChar (t ∧ anyChar) -> normRelA $ EStrAt s i1 :=: ECharA c
  -----------------------------------------------------------
  a :=: ESol x b r | Just r' <- tryEqARel a x b r -> normRelA r'  
  a :≠: ESol x b r | Just r' <- tryNeARel a x b r -> normRelA r'
  -----------------------------------------------------------
  r | [x] <- freeVars r
    , Just b <- typeOfVarInRelA x r
    , Just e <- abstract x b r
    , let r' = EVar x :=: e
    , r' < r                                  -> normRelA r'
  -----------------------------------------------------------
  r | r' <- descendBi normExprA r, r' /= r    -> normRelA r'
    | otherwise                               -> Right r

isSol :: ExprA -> Bool
isSol (ESol _ _ _) = True
isSol _            = False

pattern Range :: Inf Integer -> Inf Integer -> AInt
pattern Range a b <- (AInt.intervals -> [AInt.In a b])

-------------------------------------------------------------------------------

-- | Try to resolve equality between an expression and an abstract relation.
-- For example, @[1,∞] = {x| s[x] ≠ 'a'}@ resolves to @s[[1,∞]] = Σ∖a@.
tryEqARel :: ExprA -> Name -> Base -> RelA -> Maybe RelA
tryEqARel a x b = \case
  r | ESol x1 b1 r1 <- a            -> tryEqARel2 (x1,b1,r1) (x,b,r)
  r | isConcrete' a, x `notFreeIn` a -> Just $ subst a x r
  -----------------------------------------------------------
  EStrAt (EVar s) i :=: ECharA ĉ    -> Just $ EStrAt (EVar s) (subst a x i) :=: ECharA ĉ
  EStrAt (EVar s) i :≠: ECharA ĉ    -> Just $ EStrAt (EVar s) (subst a x i) :=: ECharA (neg ĉ)
  -----------------------------------------------------------
  EVar x1 :≠: e | x == x1, x `notFreeIn` e -> Just $ a :≠: e
  (EVar x1 :-: EIntA k) :≠: e | x == x1, x `notFreeIn` e -> Just $ (a :-: EIntA k) :≠: e
  -----------------------------------------------------------
  _                                 -> Nothing

isConcrete' :: ExprA -> Bool
isConcrete' = \case
  EUnitA Unit -> True
  EBoolA (value -> Just _) -> True
  EIntA (AInt.values -> [_]) -> True
  ECharA (AChar.values -> [_]) -> True
  EStrA (AString1 _) -> True
  EAbs _ -> False
  ESol _ _ _ -> False
  EReg _ -> False
  _ -> True

-- | Try to resolve inequality between an expressions and an abstract relation.
-- For example, @[1,∞] || {x| s[x] ≠ 'a'}@ resolves to @s[[1,∞]] ≠ Σ∖a@
tryNeARel :: ExprA -> Name -> Base -> RelA -> Maybe RelA
tryNeARel a x b r = fmap inverse $ tryEqARel a x b r

pattern EqChar :: ExprA -> AChar -> RelA
pattern EqChar e c <- (relToEqChar -> Just (e,c))

relToEqChar :: RelA -> Maybe (ExprA, AChar)
relToEqChar = \case
--   e :=: EChar  c _ -> Just (e, AChar.eq c)
   e :=: ECharA c   -> Just (e, c)
--   e :≠: EChar  c _ -> Just (e, AChar.ne c)
   e :≠: ECharA c   -> Just (e, neg c)
   _ -> Nothing

pattern VarPlusN :: Name -> Integer -> ExprA
pattern VarPlusN x n <- (exprToVarPlusN -> Just (x,n))

exprToVarPlusN :: ExprA -> Maybe (Name, Integer)
exprToVarPlusN = \case
  EVar x -> Just (x, 0)
  EVar x :+: EIntA n̂ | [n] <- AInt.values n̂ -> Just (x, n)
  _ -> Nothing

-- | Try to resolve equality between two abstract relations.
tryEqARel2 :: (Name,Base,RelA) -> (Name,Base,RelA) -> Maybe RelA
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

-- TODO: clean all of this up now that ARel is in AValue
-- TODO: track whether AValue has free vars in type?

abstractVarToValue :: Name -> Base -> RelA -> Pan AValue
abstractVarToValue x b r0 = do
  case normRelA r0 of
    Left True  -> return $ topValue b
    Left False -> return $ botValue b
    Right r -> do
      let e = abstract x b r
      unless (isNothing e) $
        logMessage $ "⟦" <> pretty r0 <> "⟧↑" <> pretty x <+> "≐" <+> pretty e
      case e of
        Just e'@(ESol _ _ _) -> throwError $ AbstractionToValueImpossible x r e'
        Just (EAbs a) -> return a
        Just e'       -> throwError $ AbstractionToValueImpossible x r e'
        Nothing       -> throwError $ AbstractionImpossible x r

abstractVar :: Name -> Base -> RelA -> Pan ExprA
abstractVar x b r0 = do
  case normRelA r0 of
    Left True  -> return $ EAbs $ topValue b
    Left False -> return $ EAbs $ botValue b
    Right r -> do
      let e = fromMaybe (ESol x b r) (abstract x b r)
      logMessage $ "⟦" <> pretty r0 <> "⟧↑" <> pretty x <+> "≐" <+> pretty e
      return e

abstract :: Name -> Base -> RelA -> Maybe ExprA
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
  -- TODO: this kind of reordering should happen during normRelA, no?
  (EStrLen s2 :+: EIntA î  ) :=: EStrFirstIndexOfChar s1 c -> abstract x b $ (EStrFirstIndexOfChar s1 c :+: EIntA (AInt.sub (AInt.eq 0)          î )) :=: EStrLen s2
  (EStrLen s2 :-: EIntA î  ) :=: EStrFirstIndexOfChar s1 c -> abstract x b $ (EStrFirstIndexOfChar s1 c :+: EIntA                                î  ) :=: EStrLen s2
  -----------------------------------------------------------
  (EStrFirstIndexOfChar (EVar s1) (ECharA ĉ  ) :+: EIntA î  ) :=: EStrLen (EVar s2) | x == s1, x == s2 -> Just $ EStrA $ strWithFirstIndexOfCharRev ĉ î
  -----------------------------------------------------------
  -- TODO: generalize these special cases
  (EStrFirstIndexOfChar (EVar s1) (ECharA ĉ1) :+: EIntA î) :=: EStrFirstIndexOfChar (EVar s2) (ECharA ĉ2)
    | [c1] <- AChar.values ĉ1, [c2] <- AChar.values ĉ2
    , x == s1, x == s2, c1 /= c2, î == î ∧ AInt.ge 0
    -> Just $ EStrA $ star (lit (AChar.ne c1 ∧ AChar.ne c2)) <> opt ((lit (AChar.eq c2) <> star (lit (AChar.ne c1))) ∨ (lit (AChar.eq c1) <> star anyChar))
  (EStrFirstIndexOfChar (EVar s1) (ECharA ĉ1) :-: EIntA î) :=: EStrFirstIndexOfChar (EVar s2) (ECharA ĉ2)
    | [c1] <- AChar.values ĉ1, [c2] <- AChar.values ĉ2
    , x == s1, x == s2, c1 /= c2, î == î ∧ AInt.le 1
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
  EVar _ :=: EStrComp (EStrA s )              -> Just $ EStrA (neg s)
  EVar _ :=: e                                -> Just e
  -----------------------------------------------------------
  EVar _ :≠: EBoolA c                         -> Just $ EBoolA (neg c)
  EVar _ :≠: e | b == TBool                   -> Just $ normExprA $ ENot e
  -----------------------------------------------------------
  EVar _ :≠: EIntA c                          -> Just $ EIntA (neg c)
  EVar _ :<: EIntA c                          -> Just $ EIntA (AInt.ltA c)
  EVar _ :≤: EIntA c                          -> Just $ EIntA (AInt.leA c)
  EVar _ :>: EIntA c                          -> Just $ EIntA (AInt.gtA c)
  EVar _ :≥: EIntA c                          -> Just $ EIntA (AInt.geA c)
  -----------------------------------------------------------
  EVar _ :<: e                                -> Just $ normExprA $ e :-: EIntA (AInt.ge 1)
  EVar _ :≤: e                                -> Just $ normExprA $ e :-: EIntA (AInt.ge 0)
  EVar _ :>: e                                -> Just $ normExprA $ e :+: EIntA (AInt.ge 1)
  EVar _ :≥: e                                -> Just $ normExprA $ e :+: EIntA (AInt.ge 0)
  -----------------------------------------------------------
  EVar _ :≠: ECharA c                         -> Just $ ECharA (neg c)
  -----------------------------------------------------------
  EVar _ :≠: EStrA s                          -> Just $ EStrA (neg s)  
  EVar _ :≠: e | b == TString                 -> Just $ EStrComp e
  -----------------------------------------------------------
  e :∈: EStrA s                               -> abstract x b $ e :=: EStrA s
  e :∈: EReg ere                              -> abstract x b $ e :=: (EStrA $ AString.fromRegex $ Regex.POSIX.ERE.toRegex ere)
  e :∉: EReg ere                              -> abstract x b $ e :≠: (EStrA $ AString.fromRegex $ Regex.POSIX.ERE.toRegex ere)
  -----------------------------------------------------------
  (EVar _ :+: EIntA c) :=: e                  -> Just $ normExprA $ e :-: EIntA c
  (EVar _ :-: EIntA c) :=: e                  -> Just $ normExprA $ e :+: EIntA c
  -----------------------------------------------------------
  EStrLen (EVar _) :=: EIntA n̂                -> Just $ EStrA $ strOfLen n̂
  EStrLen (EVar _) :<: EIntA n̂                -> Just $ EStrA $ strOfLen (AInt.ltA n̂)
  EStrLen (EVar _) :≤: EIntA n̂                -> Just $ EStrA $ strOfLen (AInt.leA n̂)
  EStrLen (EVar _) :>: EIntA n̂                -> Just $ EStrA $ strOfLen (AInt.gtA n̂)
  EStrLen (EVar _) :≥: EIntA n̂                -> Just $ EStrA $ strOfLen (AInt.geA n̂)
  -----------------------------------------------------------
  EStrLen (EVar _) :≠: EIntA n̂                -> Just $ EStrA $ strNotOfLen n̂
  -----------------------------------------------------------
  EStrAt (EVar _) (EIntA î  ) :=: ECharA ĉ   -> Just $ EStrA $ strWithCharAt î ĉ
  -----------------------------------------------------------
  EStrAt (EVar _) (EIntA î  ) :≠: ECharA ĉ   -> Just $ EStrA $ strWithoutCharAt î ĉ  
  -----------------------------------------------------------
  EStrAt (EVar s1) (EStrLen (EVar s2) :-: EIntA î  ) :=: ECharA ĉ   | x == s1, x == s2 -> Just $ EStrA $ strWithCharAtRev î ĉ
  EStrAt (EVar s1) (EStrLen (EVar s2) :+: EIntA TOP) :=: ECharA ĉ   | x == s1, x == s2 -> Just $ EStrA $ strWithCharAtRev TOP ĉ
  EStrAt (EVar s1) (EStrLen (EVar s2) :+: EIntA î  ) :=: c          | x == s1, x == s2 -> abstract x b $ EStrAt (EVar s1) (EStrLen (EVar s2) :-: EIntA (AInt.sub (AInt.eq 0) î)) :=: c
  -----------------------------------------------------------
  EStrAt (EVar s1) (EStrLen (EVar s2) :-: EIntA î  ) :≠: ECharA ĉ   | x == s1, x == s2 -> Just $ EStrA $ strWithoutCharAtRev î ĉ
  EStrAt (EVar s1) (EStrLen (EVar s2) :+: EIntA TOP) :≠: ECharA ĉ   | x == s1, x == s2 -> Just $ EStrA $ strWithoutCharAtRev TOP ĉ
  -----------------------------------------------------------
  EStrSub (EVar _) (EIntA î  ) (EIntA ĵ  ) :=: EStrA t̂   -> Just $ EStrA $ strWithSubstr î ĵ t̂
  -----------------------------------------------------------
  EStrSub (EVar _) (EIntA î  ) (EIntA ĵ  ) :≠: EStrA t̂   -> Just $ EStrA $ strWithoutSubstr î ĵ t̂
  -----------------------------------------------------------
  EStrFirstIndexOfChar (EVar _) (ECharA ĉ  ) :=: EIntA î   -> Just $ EStrA $ strWithFirstIndexOfChar ĉ î
  -----------------------------------------------------------
  EStrSub (EVar s1) (EIntA î) (EStrFirstIndexOfChar (EVar s2) (ECharA ĉ) :-: EIntA ĵ) :=: EStrA t̂
    | [i] <- AInt.values î, [j] <- AInt.values ĵ
    , x == s1, x == s2, i >= 0, j >= 0 -> Just $ EStrA $ rep c̄ i <> (t̂ ∧ star c̄) <> rep c̄ (j-1) <> lit ĉ <> star anyChar
    where
      c̄ = lit (neg ĉ)
  -----------------------------------------------------------
  EStrSub (EVar s1) (EStrFirstIndexOfChar (EVar s2) (ECharA ĉ) :+: EIntA î) (EStrLen (EVar s3) :-: EIntA ĵ) :=: EStrA t̂
    | [i] <- AInt.values î, [j] <- AInt.values ĵ
    , x == s1, x == s2, x == s3 -> Just $ EStrA $ strWithSubstrFromFirstIndexOfCharToEnd ĉ i j t̂
  -----------------------------------------------------------
  EStrIndexOf (EVar _) (EStrA (AString.toChar -> Just  ĉ )  ) (EIntA AInt0) :=: EIntA î   -> Just $ EStrA $ strWithFirstIndexOfChar ĉ î
  -----------------------------------------------------------
  EStrIndexOf (EVar s1) (EStrA (AString.toChar -> Just ĉ1)) (EStrIndexOf (EVar s2) (EStrA (AString.toChar -> Just ĉ2)) (EIntA AInt0) :+: EIntA AInt1) :=: EIntA k̂
    | x == s1, s1 == s2 -> Just $ EStrA $ strWithFirstIndexOfCharFollowedByFirstIndexOfChar ĉ2 ĉ1 k̂
  -----------------------------------------------------------
  EStrAt (EVar s1) (EStrIndexOf (EVar s2) (EStrA (AString.toChar -> Just ĉ1)) (EIntA î) :+: EIntA n̂) :=: ECharA ĉ2
    | [i] <- AInt.values î, [n] <- AInt.values n̂
    , x == s1, x == s2
    , let c̄1 = neg ĉ1
    -> Just $ EStrA $ rep anyChar i <> star (lit c̄1) <> lit ĉ1 <> rep anyChar (n - 1) <> lit ĉ2 <> star anyChar
  -----------------------------------------------------------
  EStrContains (EVar _) (EStrA s ) :=: EBoolA (ABool.value -> Just doesContain)
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
  [Fin m  :..: PosInf]                      -> mk (:≥:) m
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
    Nothing  -> panic $ "cannot convert Regex to ERE:" <+> prettyRegex r
