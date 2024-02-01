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
  Rel _ e1 e2 | x ∉ e1, x ∉ e2 -> Left r
  Rel _ e1 e2 | x ∉ e1, x ∈ e2 -> maybe (Left r) (abstract x b) (converse r)

  -- here, x may be on both sides ---------------------------------------------

  EVar x1 :=: EVar x2 | x1 == x2 -> Right $ topExpr b

  ENot e1 :≠: e2 -> abstract x b $ e1 :=: e2
  e1 :≠: ENot e2 -> abstract x b $ e1 :=: e2

  EFun "_IntComplement" [e1] :≠: e2 | e1 == e2 -> abstract x b $ e1 :=: e2

  EFun "_StrComplement" [e1] :≠: EFun "_StrComplement" [e2] -> abstract x b $ e1 :≠: e2

  (EFun "_StrAt_index" [s,c]      ) :=: i -> abstract x b $ EStrAt s         i        :=: c
  (EFun "_StrAt_index" [s,c] :+: y) :=: i -> abstract x b $ EStrAt s (norm $ i :-: y) :=: c
  (EFun "_StrAt_index" [s,c] :-: y) :=: i -> abstract x b $ EStrAt s (norm $ i :+: y) :=: c
  i :=: (EFun "_StrAt_index" [s,c]      ) -> abstract x b $ EStrAt s         i        :=: c
  i :=: (EFun "_StrAt_index" [s,c] :+: y) -> abstract x b $ EStrAt s (norm $ i :-: y) :=: c
  i :=: (EFun "_StrAt_index" [s,c] :-: y) -> abstract x b $ EStrAt s (norm $ i :+: y) :=: c

  (EFun "_StrAt_index" [s,c]      ) :≠: i -> abstract x b $ EStrAt s         i        :≠: c
  (EFun "_StrAt_index" [s,c] :+: y) :≠: i -> abstract x b $ EStrAt s (norm $ i :-: y) :≠: c
  (EFun "_StrAt_index" [s,c] :-: y) :≠: i -> abstract x b $ EStrAt s (norm $ i :+: y) :≠: c
  i :≠: (EFun "_StrAt_index" [s,c]      ) -> abstract x b $ EStrAt s         i        :≠: c
  i :≠: (EFun "_StrAt_index" [s,c] :+: y) -> abstract x b $ EStrAt s (norm $ i :-: y) :≠: c
  i :≠: (EFun "_StrAt_index" [s,c] :-: y) -> abstract x b $ EStrAt s (norm $ i :+: y) :≠: c

  (EFun "_StrSub_index_end" [s,t,i] :+: y) :=: j 
    -> abstract x b $ EStrSub s i (norm $ j :-: y) :=: t

  EStrAt (EVar x1) (EStrLen (EVar x2) :-: EIntA n) :=: ECharA c | x == x1, x == x2 
    -> Right $ EStrA $ absStrAtRev n c

  EStrSub s (EInt 0 _) (EStrLen (EVar x1) :-: EInt 1 _) :=: EFun "_StrComplement" [EVar x2]
    | x ∉ s, x == x1, x == x2
    -> Right $ EFun "_StrComplement" [EStrSub s (EInt 0 NoPV) (EIntA $ AInt.ge 0)]

  EStrSub s (EInt 0 _) (EStrLen (EVar x1) :-: EInt 1 _) :≠: EVar x2
    | x ∉ s, x == x1, x == x2
    -> Right $ EFun "_StrComplement" [EStrSub s (EInt 0 NoPV) (EIntA $ AInt.ge 0)]


  EStrSub (EVar x1) (EInt 0 _) (EIntA j) :≠: EStrSub (EVar x2) (EInt 0 _) (EIntA l)
    | x == x1, x == x2, not $ isBot $ j ∧ l
    -> Right $ EStrA bot

  Rel _ e1 e2 | x ∈ e1, x ∈ e2 -> Left r

  -- below, x occurs only on LHS (but possibly more than once) ----------------

  EVar _x :=: e -> Right e

  Rel o (ENot e1) e2 -> abstract x b $ Rel o e1 (norm $ ENot e2)

  Rel o (e1 :+: e2) e3 | x ∉ e2 -> abstract x b $ Rel o e1 (norm $ e3 :-: e2)
  Rel o (e1 :+: e2) e3 | x ∉ e1 -> abstract x b $ Rel o e2 (norm $ e3 :-: e1)
  Rel o (e1 :-: e2) e3 | x ∉ e2 -> abstract x b $ Rel o e1 (norm $ e3 :+: e2)
  Rel o (e1 :-: e2) e3 | x ∉ e1 -> abstract x b $ Rel o e2 (norm $ e1 :-: e3)
  
  e :≥: EIntA n -> abstract x b $ e :=: EIntA (AInt.add n (AInt.ge 0))
  e :>: EIntA n -> abstract x b $ e :=: EIntA (AInt.add n (AInt.ge 1))
  e :≤: EIntA n -> abstract x b $ e :=: EIntA (AInt.sub n (AInt.ge 0))
  e :<: EIntA n -> abstract x b $ e :=: EIntA (AInt.sub n (AInt.ge 1))

  e1 :≥: e2 -> abstract x b $ e1 :=: (e2 :+: EIntA (AInt.ge 0))
  e1 :>: e2 -> abstract x b $ e1 :=: (e2 :+: EIntA (AInt.ge 1))
  e1 :≤: e2 -> abstract x b $ e1 :=: (e2 :-: EIntA (AInt.ge 0))
  e1 :<: e2 -> abstract x b $ e1 :=: (e2 :-: EIntA (AInt.ge 1))

  EVar _x :≠: EBoolA a -> Right $ EBoolA (neg a)
  EVar _x :≠: EIntA  n -> Right $ EIntA  (neg n)  
  EVar _x :≠: ECharA c -> Right $ ECharA (neg c)
  EVar _x :≠: EStrA  s -> Right $ EStrA  (neg s)

  EVar _x :≠: EVar y | b == TBool   -> Right $ ENot (EVar y)
  EVar _x :≠: EVar y | b == TInt    -> Right $ EFun "_IntComplement" [EVar y]
  EVar _x :≠: EVar y | b == TString -> Right $ EFun "_StrComplement" [EVar y]

  EVar _x :∈: EReg ere -> Right $ EStrA $ AString.fromRegex $ Regex.POSIX.ERE.toRegex ere
  EVar _x :∉: EReg ere -> Right $ EStrA $ neg $ AString.fromRegex $ Regex.POSIX.ERE.toRegex ere

  EStrLen s :≠: EIntA n -> abstract x b $ EStrLen s :=: EIntA (diff (AInt.ge 0) n)

  EStrLen (EVar _x) :=: EIntA n -> Right $ EStrA $ absStrLen n

  EStrAt e1 e2 :≠: ECharA c -> abstract x b $ EStrAt e1 e2 :=: ECharA (neg c)

  EStrAt (EVar _x) (EIntA i) :=: ECharA c -> Right $ EStrA $ absStrAt i c

  EStrAt s (EVar _x      ) :=: c | x ∉ s        -> Right $ EFun "_StrAt_index" [s,c]
  EStrAt s (EVar _x :+: y) :=: c | x ∉ s, x ∉ y -> Right $ EFun "_StrAt_index" [s,c] :-: y
  EStrAt s (EVar _x :-: y) :=: c | x ∉ s, x ∉ y -> Right $ EFun "_StrAt_index" [s,c] :+: y

  EStrAt s (EVar _x) :≠: ec | x ∉ s -> case ec of
    EChar  c _ -> Right $ EFun "_StrAt_index" [s, ECharA (AChar.ne c)]
    ECharA c   -> Right $ EFun "_StrAt_index" [s, ECharA (neg c)]
    _          -> Left r

  EStrSub s i (EVar _      ) :=: t | x ∉ s, x ∉ i -> Right $ EFun "_StrSub_index_end" [s,t,i]
  EStrSub s i (EVar _ :-: y) :=: t | x ∉ s, x ∉ i -> Right $ EFun "_StrSub_index_end" [s,t,i] :+: y

  -- TODO: higher functions, e.g.: prefixof, prefixes etc?
  EStrSub (EVar _) (EInt 0 _) (EIntA j) :=: EStrA t
    | (j ∧ AInt.ge 0) == AInt.ge 0 
    -> Right $ EStrA $ t <> star anyChar

  EStrSub s i j :≠: EStrA t -> abstract x b $ EStrSub s i j :=: EStrA (neg t)

  EFun "_StrComplement" [s] :=: e -> abstract x b $ s :=: (norm $ EFun "_StrComplement" [e])
  EFun "_StrComplement" [s] :≠: e -> abstract x b $ s :≠: (norm $ EFun "_StrComplement" [e])

  _ -> Left r

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
