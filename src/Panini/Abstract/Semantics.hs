module Panini.Abstract.Semantics where

import Algebra.Lattice
import Control.Monad
import Panini.Abstract.AUnit as AUnit
import Panini.Abstract.ABool as ABool
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
import Panini.Regex.POSIX.ERE qualified as Regex.POSIX.ERE
import Panini.Syntax
import Prelude
import Panini.Regex qualified as Regex

-- local notation -------------------------------------------------------------

(∈) :: Subable a v => Name -> a -> Bool
x ∈ e = x `elem` freeVars e

(∉) :: Subable a v => Name -> a -> Bool
x ∉ e = x `notElem` freeVars e

-------------------------------------------------------------------------------

abstractVar :: Name -> Base -> Rel -> Pan AExpr
abstractVar x b r = case abstract x b r of
  Left r2 -> throwError $ AbstractionImpossible x r r2
  Right e -> do
    logMessage $ "⟦" <> pretty r <> "⟧↑" <> pretty x <+> "≐" <+> pretty e
    return e

abstract :: Name -> Base -> Rel -> Either Rel AExpr
abstract x b r = case normRel r of
  e1 :⋈: e2 | x ∉ e1, x ∉ e2 -> Left r
  e1 :⋈: e2 | x ∉ e1, x ∈ e2 -> maybe (Left r) (abstract x b) (converse r)

  -- here, x occurs on LHS and MAY also occur on RHS --------------------------

  e1 :=: e2 | normA e1 == normA e2 -> Right $ topExpr b
  e1 :≠: e2 | normA e1 == normA e2 -> Right $ botExpr b

  Rel o (e1 :+: e2) e3 | x ∉ e2 -> abstract x b $ Rel o e1 (norm $ e3 :-: e2)
  Rel o (e1 :+: e2) e3 | x ∉ e1 -> abstract x b $ Rel o e2 (norm $ e3 :-: e1)
  Rel o (e1 :-: e2) e3 | x ∉ e2 -> abstract x b $ Rel o e1 (norm $ e3 :+: e2)
  Rel o (e1 :-: e2) e3 | x ∉ e1 -> abstract x b $ Rel o e2 (norm $ e1 :-: e3)

  (StrSub_index2 s t i :+: y) :=: j 
    -> abstract x b $ EStrSub s i (norm $ j :-: y) :=: t

  EStrLen _ :≠: (EStrLen _ :+: EIntA a) | not (AInt.member 0 a) -> Right $ topExpr b

  EStrAt (EVar x1) (EStrLen (EVar x2) :-: EIntA n) :=: ECharA c 
    | x == x1, x == x2 
    -> Right $ EStrA $ absStrAtRev n c

  EStrSub s Int0 (EStrLen (EVar x1) :-: Int1) :=: StrComp (EVar x2)
    | x ∉ s, x == x1, x == x2
    -> Right $ StrComp (EStrSub s Int0 (EIntA $ AInt.ge 0))

  EStrSub s Int0 (EStrLen (EVar x1) :-: Int1) :≠: EVar x2
    | x ∉ s, x == x1, x == x2
    -> Right $ StrComp (EStrSub s Int0 (EIntA $ AInt.ge 0))

  EStrSub (EVar x1) Int0 (EIntA j) :≠: EStrSub (EVar x2) Int0 (EIntA l)
    | x == x1, x == x2, not $ isBot $ j ∧ l
    -> Right $ EStrA bot

  ENot e1 :=: ENot e2 -> abstract x b $ e1 :=: e2
  ENot e1 :=: e2      -> abstract x b $ e1 :≠: e2
  e1      :=: ENot e2 -> abstract x b $ e1 :≠: e2
  ENot e1 :≠: ENot e2 -> abstract x b $ e1 :≠: e2
  ENot e1 :≠: e2      -> abstract x b $ e1 :=: e2
  e1      :≠: ENot e2 -> abstract x b $ e1 :=: e2

  IntComp e1 :=: IntComp e2 -> abstract x b $ e1 :=: e2
  IntComp e1 :=: e2         -> abstract x b $ e1 :≠: e2
  e1         :=: IntComp e2 -> abstract x b $ e1 :≠: e2
  IntComp e1 :≠: IntComp e2 -> abstract x b $ e1 :≠: e2
  IntComp e1 :≠: e2         | e1 == e2 -> Right $ topExpr b
  e1         :≠: IntComp e2 | e1 == e2 -> Right $ topExpr b

  StrComp e1 :=: StrComp e2 -> abstract x b $ e1 :=: e2
  StrComp e1 :=: e2         -> abstract x b $ e1 :≠: e2
  e1         :=: StrComp e2 -> abstract x b $ e1 :≠: e2
  StrComp e1 :≠: StrComp e2 -> abstract x b $ e1 :≠: e2    
  StrComp e1 :≠: e2         | e1 == e2 -> Right $ topExpr b
  e1         :≠: StrComp e2 | e1 == e2 -> Right $ topExpr b

  e1 :⋈: e2 | x ∈ e1, x ∈ e2 -> Left r

  -- below, x occurs only on LHS (but possibly more than once) ----------------

  EVar _ :=: e -> Right e
  
  e1 :≥: e2 -> abstract x b $ e1 :=: (norm $ e2 :+: EIntA (AInt.ge 0))
  e1 :>: e2 -> abstract x b $ e1 :=: (norm $ e2 :+: EIntA (AInt.ge 1))
  e1 :≤: e2 -> abstract x b $ e1 :=: (norm $ e2 :-: EIntA (AInt.ge 0))
  e1 :<: e2 -> abstract x b $ e1 :=: (norm $ e2 :-: EIntA (AInt.ge 1))

  EVar _ :≠: EUnitA a -> Right $ EUnitA (neg a)
  EVar _ :≠: EBoolA a -> Right $ EBoolA (neg a)
  EVar _ :≠: EIntA  a -> Right $ EIntA  (neg a)
  EVar _ :≠: ECharA a -> Right $ ECharA (neg a)
  EVar _ :≠: EStrA  a -> Right $ EStrA  (neg a)

  EVar _ :≠: e | b == TBool   -> Right $ ENot    e
  EVar _ :≠: e | b == TInt    -> Right $ IntComp e
  EVar _ :≠: e | b == TString -> Right $ StrComp e

  EVar _ :∈: EReg ere -> Right $ EStrA $       absStrERE ere
  EVar _ :∉: EReg ere -> Right $ EStrA $ neg $ absStrERE ere

  EStrLen s :≠: EIntA n -> abstract x b $ EStrLen s :=: EIntA (diff (AInt.ge 0) n)

  EStrLen (EVar _) :=: EIntA n -> Right $ EStrA $ absStrLen n

  EStrAt e1 e2 :≠: ECharA c -> abstract x b $ EStrAt e1 e2 :=: ECharA (neg c)

  EStrAt (EVar _) (EIntA i) :=: ECharA c -> Right $ EStrA $ absStrAt i c

  EStrAt s (EVar _      ) :=: c | x ∉ s        -> Right $ StrAt_index s c
  EStrAt s (EVar _ :+: y) :=: c | x ∉ s, x ∉ y -> Right $ StrAt_index s c :-: y
  EStrAt s (EVar _ :-: y) :=: c | x ∉ s, x ∉ y -> Right $ StrAt_index s c :+: y

  EStrAt s (EVar _) :≠: ECharA c | x ∉ s -> Right $ StrAt_index s (ECharA (neg c))

  EStrSub s i (EVar _      ) :=: t | x ∉ s, x ∉ i -> Right $ StrSub_index2 s t i
  EStrSub s i (EVar _ :+: y) :=: t | x ∉ s, x ∉ i -> Right $ StrSub_index2 s t i :-: y
  EStrSub s i (EVar _ :-: y) :=: t | x ∉ s, x ∉ i -> Right $ StrSub_index2 s t i :+: y

  -- TODO: check that length t == (j - i) + 1
  EStrSub (EVar _) (EIntA i0) (EIntA j0) :=: EStrA t
    | [i] <- AInt.values i0, [j] <- AInt.values j0, j >= i
    -> Right $ EStrA $ rep anyChar i <> t <> star anyChar

  EStrSub (EVar _) Int0 (EIntA j) :=: EStrA t
    | (j ∧ AInt.ge 0) == AInt.ge 0 
    -> Right $ EStrA $ t <> star anyChar

  EStrSub s ei@(EIntA i0) ej@(EIntA j0) :≠: EStrA t
    | [n] <- AInt.values $ AInt.add (AInt.eq 1) $ AInt.sub j0 i0
    -> abstract x b $ EStrSub s ei ej :=: EStrA (neg t ∧ rep anyChar n)

  StrComp s :≠: e -> abstract x b $ s :≠: (norm $ StrComp e)

  _ -> Left r

absStrERE :: Regex.POSIX.ERE.ERE -> AString
absStrERE = AString.fromRegex . Regex.POSIX.ERE.toRegex

absStrLen :: AInt -> AString
absStrLen (meet (AInt.ge 0) -> n)
  | isBot n   = bot
  | otherwise = joins $ flip concatMap (AInt.intervals n) $ \case
      AInt.In (Fin a) (Fin b) -> [rep anyChar i | i <- [a..b]]
      AInt.In (Fin a) PosInf  -> [rep anyChar a <> star anyChar]
      _                       -> impossible  

absStrAt :: AInt -> AChar -> AString
absStrAt (meet (AInt.ge 0) -> n) c
  | isBot n = bot
  | isBot c = undefined -- TODO
  | otherwise = go 0 (AInt.intervals n)
  where
    go i (AInt.In (Fin a) (Fin b) : xs) = rep anyChar (a - i) <> rep (lit c) (b - a + 1) <> go b xs
    go i (AInt.In (Fin a) PosInf  : []) = rep anyChar (a - i) <> star (lit c)
    go _ []                             = star anyChar
    go _ _                              = impossible

absStrAtRev :: AInt -> AChar -> AString
absStrAtRev (meet (AInt.ge 1) -> n) c
  | isBot n   = bot
  | isBot c   = bot
  | otherwise = go 1 (AInt.intervals n)
  where
    go i (AInt.In (Fin a) (Fin b) : xs) = go b xs <> rep (lit c) (b - a + 1) <> rep anyChar (a - i)
    go i (AInt.In (Fin a) PosInf  : []) = star (lit c) <> rep anyChar (a - i)
    go _ []                             = star anyChar
    go _ _                              = impossible

pattern Int0 :: Expr
pattern Int0 <- (isN 0 -> True) where
  Int0 = EInt 0 NoPV

pattern Int1 :: Expr
pattern Int1 <- (isN 1 -> True) where
  Int1 = EInt 1 NoPV

isN :: Integer -> Expr -> Bool
isN n (EInt  m _) = m == n
isN n (EIntA a)   = AInt.values a == [n]
isN _ _           = False

-------------------------------------------------------------------------------

concretizeVar :: Name -> Base -> AValue -> Pan Pred
concretizeVar x b v = logAndReturn $ case (b,v) of
  (TUnit  , AUnit   a) -> concretizeUnit   x a
  (TBool  , ABool   a) -> concretizeBool   x a
  (TInt   , AInt    a) -> concretizeInt    x a
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
  [NegInf :..: Fin n ]                      -> mk Le n
  [Fin m  :..: PosInf]                      -> mk Ge m
  [Fin m  :..: Fin n ] | m == n             -> mk Eq m
                       | otherwise          -> mk Ge m ∧ mk Le n
  [NegInf :..: Fin m, Fin n :..: PosInf]
                               | n - m == 2 -> mk Ne (m + 1)
                               | otherwise  -> mk Le m ∨ mk Ge n
  (Fin m  :..: _) : (last -> _ :..: Fin n ) -> mk Ge m ∧ mk Le n ∧ mkHoles
  (NegInf :..: _) : (last -> _ :..: Fin n ) -> mk Le n ∧ mkHoles
  (Fin m  :..: _) : (last -> _ :..: PosInf) -> mk Ge m ∧ mkHoles
  (NegInf :..: _) : (last -> _ :..: PosInf) -> mkHoles
  _                                         -> impossible
 where
  mk op n  = PRel $ Rel op (EVar x) (EInt n NoPV)
  mkHoles  = meets $ map (mk Ne) $ AInt.holes $ AInt.intervals a

-- TODO: move to AInt module
pattern (:..:) :: Inf Integer -> Inf Integer -> AInt.Interval
pattern a :..: b = AInt.In a b

concretizeString :: Name -> AString -> Pred
concretizeString x a = case AString.toRegex a of
  Regex.Zero -> PFalse
  Regex.One  -> PRel $ EVar x :=: EStr "" NoPV
  Regex.All  -> PTrue
  r -> case Regex.POSIX.ERE.fromRegex r of
    Just ere -> PRel $ EVar x :∈: EReg ere
    Nothing  -> panic $ "cannot convert Regex to ERE:" <+> pretty r
