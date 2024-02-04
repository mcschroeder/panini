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

concretizeVar :: Name -> Base -> AValue -> Pan Pred
concretizeVar x TUnit (AUnit a) = case a of
  AUnit.Unit -> return $ PRel $ EVar x :=: EUnit NoPV
  AUnit.Bottom -> return PFalse

concretizeVar x TBool (ABool a) = case ABool.value a of
  Just b  -> return $ PRel $ EVar x :=: EBool b NoPV
  Nothing -> if isTop a then return PTrue else return PFalse

-- TODO
concretizeVar x TInt (AInt a) 
  | AInt.continuous a = case (AInt.minimum a, AInt.maximum a) of
      (Just NegInf, Just PosInf) -> return PTrue
      (Just NegInf, Just (Fin n)) -> return $ PRel $ EVar x :≤: EInt n NoPV
      (Just (Fin m), Just PosInf) -> return $ PRel $ EVar x :≥: EInt m NoPV
      (Just (Fin m), Just (Fin n)) 
        | m == n -> return $ PRel $ EVar x :=: EInt m NoPV
        | otherwise -> return $ PAnd [ PRel $ EVar x :≥: EInt m NoPV
                                     , PRel $ EVar x :≤: EInt n NoPV ]
      (Nothing, Nothing) -> return PFalse
      _ -> impossible
  
  | [AInt.In NegInf (Fin m), AInt.In (Fin n) PosInf] <- AInt.intervals a =
      if n - m == 2
        then return $ PRel $ EVar x :≠: EInt (m + 1) NoPV
        else return $ POr [ PRel $ EVar x :≤: EInt m NoPV
                          , PRel $ EVar x :≥: EInt n NoPV ]
  
  | Just n <- AInt.count a, n < 5 =  -- TODO: arbitrary cutoff
      return $ POr $ map (\i -> PRel $ EVar x :=: EInt i NoPV) $ AInt.values a

concretizeVar x TString (AString a) = case AString.toRegex a of
  Regex.Zero -> return PFalse
  Regex.One  -> return $ PRel $ EVar x :=: EStr "" NoPV
  Regex.All  -> return $ PTrue
  r -> case Regex.POSIX.ERE.fromRegex r of
    Just ere -> return $ PRel $ EVar x :∈: EReg ere
    Nothing  -> panic $ "cannot convert Regex to ERE:" <+> pretty r

concretizeVar x b a = throwError $ ConcretizationImpossible x b a

abstractVar :: Name -> Base -> Rel -> Pan AExpr
abstractVar x b r = case abstract x b r of
  Left r2 -> throwError $ AbstractionImpossible x r r2
  Right e -> do
    logMessage $ "⟦" <> pretty r <> "⟧↑" <> pretty x <+> "≐" <+> pretty e
    return e

topExpr :: Base -> AExpr
topExpr TBool   = EAbs $ ABool top
topExpr TInt    = EAbs $ AInt top
topExpr TString = EAbs $ AString top
topExpr TUnit   = ECon (U NoPV)

botExpr :: Base -> AExpr
botExpr TBool   = EAbs $ ABool bot
botExpr TInt    = EAbs $ AInt bot
botExpr TString = EAbs $ AString bot
botExpr b       = panic $ "no" <+> symBot <+> "for " <+> pretty b

-------------------------------------------------------------------------------

abstract :: Name -> Base -> Rel -> Either Rel AExpr
abstract x b r = case r of
  e1 :⋈: e2 | x ∉ e1, x ∉ e2 -> Left r
  e1 :⋈: e2 | x ∉ e1, x ∈ e2 -> maybe (Left r) (abstract x b) (converse r)

  -- here, x occurs on LHS and MAY also occur on RHS --------------------------

  EVar x1 :=: EVar x2 | x1 == x2 -> Right $ topExpr b
  EVar x1 :≠: EVar x2 | x1 == x2 -> Right $ botExpr b

  ENot e1 :≠: e2      -> abstract x b $ e1 :=: e2
  e1      :≠: ENot e2 -> abstract x b $ e1 :=: e2

  IntComp e1 :≠: e2         | e1 == e2 -> abstract x b $ e1 :=: e2
  e1         :≠: IntComp e2 | e1 == e2 -> abstract x b $ e1 :=: e2

  StrComp e1 :≠: StrComp e2 -> abstract x b $ e1 :≠: e2

  (StrAt_index s c      ) :=: i -> abstract x b $ EStrAt s         i        :=: c
  (StrAt_index s c :+: y) :=: i -> abstract x b $ EStrAt s (norm $ i :-: y) :=: c
  (StrAt_index s c :-: y) :=: i -> abstract x b $ EStrAt s (norm $ i :+: y) :=: c
  i :=: (StrAt_index s c      ) -> abstract x b $ EStrAt s         i        :=: c
  i :=: (StrAt_index s c :+: y) -> abstract x b $ EStrAt s (norm $ i :-: y) :=: c
  i :=: (StrAt_index s c :-: y) -> abstract x b $ EStrAt s (norm $ i :+: y) :=: c

  (StrAt_index s c      ) :≠: i -> abstract x b $ EStrAt s         i        :≠: c
  (StrAt_index s c :+: y) :≠: i -> abstract x b $ EStrAt s (norm $ i :-: y) :≠: c
  (StrAt_index s c :-: y) :≠: i -> abstract x b $ EStrAt s (norm $ i :+: y) :≠: c
  i :≠: (StrAt_index s c      ) -> abstract x b $ EStrAt s         i        :≠: c
  i :≠: (StrAt_index s c :+: y) -> abstract x b $ EStrAt s (norm $ i :-: y) :≠: c
  i :≠: (StrAt_index s c :-: y) -> abstract x b $ EStrAt s (norm $ i :+: y) :≠: c

  (StrSub_index2 s t i :+: y) :=: j 
    -> abstract x b $ EStrSub s i (norm $ j :-: y) :=: t

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

  e1 :⋈: e2 | x ∈ e1, x ∈ e2 -> Left r

  -- below, x occurs only on LHS (but possibly more than once) ----------------

  EVar _ :=: e -> Right e

  ENot e1 :=: e2 -> abstract x b $ e1 :=: (norm $ ENot e2)

  Rel o (e1 :+: e2) e3 | x ∉ e2 -> abstract x b $ Rel o e1 (norm $ e3 :-: e2)
  Rel o (e1 :+: e2) e3 | x ∉ e1 -> abstract x b $ Rel o e2 (norm $ e3 :-: e1)
  Rel o (e1 :-: e2) e3 | x ∉ e2 -> abstract x b $ Rel o e1 (norm $ e3 :+: e2)
  Rel o (e1 :-: e2) e3 | x ∉ e1 -> abstract x b $ Rel o e2 (norm $ e1 :-: e3)
  
  e1 :≥: e2 -> abstract x b $ e1 :=: (norm $ e2 :+: EIntA (AInt.ge 0))
  e1 :>: e2 -> abstract x b $ e1 :=: (norm $ e2 :+: EIntA (AInt.ge 1))
  e1 :≤: e2 -> abstract x b $ e1 :=: (norm $ e2 :-: EIntA (AInt.ge 0))
  e1 :<: e2 -> abstract x b $ e1 :=: (norm $ e2 :-: EIntA (AInt.ge 1))

  EVar _ :≠: EUnitA a -> Right $ EUnitA (neg a)
  EVar _ :≠: EBoolA a -> Right $ EBoolA (neg a)
  EVar _ :≠: EIntA  a -> Right $ EIntA  (neg a)
  EVar _ :≠: ECharA a -> Right $ ECharA (neg a)
  EVar _ :≠: EStrA  a -> Right $ EStrA  (neg a)

  EVar _ :≠: EVar y | b == TBool   -> Right $ ENot    (EVar y)
  EVar _ :≠: EVar y | b == TInt    -> Right $ IntComp (EVar y)
  EVar _ :≠: EVar y | b == TString -> Right $ StrComp (EVar y)

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

  EStrSub (EVar _) Int0 (EIntA j) :=: EStrA t
    | (j ∧ AInt.ge 0) == AInt.ge 0 
    -> Right $ EStrA $ t <> star anyChar

  EStrSub s i j :≠: EStrA t -> abstract x b $ EStrSub s i j :=: EStrA (neg t)

  StrComp s :=: e -> abstract x b $ s :=: (norm $ StrComp e)
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

pattern (:⋈:) :: Expr -> Expr -> Rel
pattern e1 :⋈: e2 <- Rel _ e1 e2

isN :: Integer -> Expr -> Bool
isN n (EInt  m _) = m == n
isN n (EIntA a)   = AInt.values a == [n]
isN _ _           = False

pattern Int0 :: Expr
pattern Int0 <- (isN 0 -> True) where
  Int0 = EInt 0 NoPV

pattern Int1 :: Expr
pattern Int1 <- (isN 1 -> True) where
  Int1 = EInt 1 NoPV

pattern StrAt_index :: Expr -> Expr -> Expr
pattern StrAt_index s c = EFun "_StrAt_index" [s,c]

pattern StrSub_index2 :: Expr -> Expr -> Expr -> Expr
pattern StrSub_index2 s t i = EFun "_StrSub_index_end" [s,t,i]

pattern StrComp :: Expr -> Expr
pattern StrComp e = EFun "_StrComplement" [e]

pattern IntComp :: Expr -> Expr
pattern IntComp e = EFun "_IntComplement" [e]
