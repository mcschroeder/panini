module Panini.Abstract.Semantics where

import Algebra.Lattice
import Control.Monad
import Data.Text qualified as Text
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
import Panini.Syntax
import Prelude

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

concretizeVar :: Name -> AExpr -> Pan Rel
concretizeVar x e = case e of
  EStrA s -> return $ EVar x :∈: EStrA s  
  _       -> throwError $ ConcretizationImpossible e x

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

  -- here, x may be on both sides ---------------------------------------------

  EFun "_StrComp" [e1] :=: EFun "_StrComp" [e2] -> abstract x b $ e1 :=: e2
  EFun "_StrComp" [e1] :=: e2                   -> abstract x b $ e1 :≠: e2
  e1                   :=: EFun "_StrComp" [e2] -> abstract x b $ e1 :≠: e2

  (EFun "_StrSub_index_end" [s,t,i]      ) :=: j -> abstract x b $ EStrSub s i         j        :=: t
  (EFun "_StrSub_index_end" [s,t,i] :+: y) :=: j -> abstract x b $ EStrSub s i (norm $ j :-: y) :=: t
  (EFun "_StrSub_index_end" [s,t,i] :-: y) :=: j -> abstract x b $ EStrSub s i (norm $ j :+: y) :=: t
  j :=: (EFun "_StrSub_index_end" [s,t,i]      ) -> abstract x b $ EStrSub s i         j        :=: t
  j :=: (EFun "_StrSub_index_end" [s,t,i] :+: y) -> abstract x b $ EStrSub s i (norm $ j :-: y) :=: t
  j :=: (EFun "_StrSub_index_end" [s,t,i] :-: y) -> abstract x b $ EStrSub s i (norm $ j :+: y) :=: t

  (EFun "_StrAt_index" [s,c]      ) :=: i -> abstract x b $ EStrAt s         i        :=: c
  (EFun "_StrAt_index" [s,c] :+: y) :=: i -> abstract x b $ EStrAt s (norm $ i :-: y) :=: c
  (EFun "_StrAt_index" [s,c] :-: y) :=: i -> abstract x b $ EStrAt s (norm $ i :+: y) :=: c
  i :=: (EFun "_StrAt_index" [s,c]      ) -> abstract x b $ EStrAt s         i        :=: c
  i :=: (EFun "_StrAt_index" [s,c] :+: y) -> abstract x b $ EStrAt s (norm $ i :-: y) :=: c
  i :=: (EFun "_StrAt_index" [s,c] :-: y) -> abstract x b $ EStrAt s (norm $ i :+: y) :=: c

  EStrSub s (EInt 0 _) (EStrLen (EVar _x1) :+: EInt (-1) _) :≠: EVar _x2 | x ∉ s, _x1 == _x2 -> Right $ EFun "_StrComp" [EStrSub s (EInt 0 NoPV) (EIntA $ AInt.ge 0)]

  EStrSub (EVar _x1) (EInt 0 _) (EIntA j1) :=: EStrSub (EVar _x2) (EInt 0 _) (EIntA j2) 
    | _x1 == _x2, j1 ∧ AInt.ge 0 == AInt.ge 0, j2 ∧ AInt.ge 0 == AInt.ge 0
    -> Right $ EStrA top

  Rel _ e1 e2 | x ∉ e1, x ∈ e2 -> maybe (Left r) (abstract x b) (converse r)
  Rel _ e1 e2 | x ∈ e1, x ∈ e2 -> Left r

  -- below, x occurs only on LHS (but possibly more than once) ----------------

  (e1 :+: e2) :=: e3 | x ∉ e2 -> abstract x b $ e1 :=: (norm $ e3 :-: e2)
  (e1 :-: e2) :=: e3 | x ∉ e2 -> abstract x b $ e1 :=: (norm $ e3 :+: e2)

  e :≠: EBool  c _ -> abstract x b $ e :=: EBool  (not c) NoPV
  e :≠: EBoolA c   -> abstract x b $ e :=: EBoolA (neg c)

  e :≠: EInt  n _ -> abstract x b $ e :=: EIntA (AInt.ne n)
  e :≥: EInt  n _ -> abstract x b $ e :=: EIntA (AInt.ge n)
  e :>: EInt  n _ -> abstract x b $ e :=: EIntA (AInt.gt n)
  e :≤: EInt  n _ -> abstract x b $ e :=: EIntA (AInt.le n)
  e :<: EInt  n _ -> abstract x b $ e :=: EIntA (AInt.lt n)

  e :≠: EIntA n -> abstract x b $ e :=: EIntA (neg n)
  e :≥: EIntA n -> abstract x b $ e :=: EIntA (AInt.add n (AInt.ge 0))
  e :>: EIntA n -> abstract x b $ e :=: EIntA (AInt.add n (AInt.ge 1))
  e :≤: EIntA n -> abstract x b $ e :=: EIntA (AInt.sub n (AInt.ge 0))
  e :<: EIntA n -> abstract x b $ e :=: EIntA (AInt.sub n (AInt.ge 1))

  -- TODO: we currently treat ALL singleton strings as chars; this is wrong!
  e :≠: EChar c _ -> abstract x b $ e :=: EStrA (lit (AChar.ne c))

  EVar _ :≠: e | b == TString -> Right $ EFun "_StrComp" [e]  

  EVar _ :≥: e | b == TInt -> Right $ e :+: EIntA (AInt.ge 0)
  EVar _ :>: e | b == TInt -> Right $ e :+: EIntA (AInt.ge 1)
  EVar _ :≤: e | b == TInt -> Right $ e :-: EIntA (AInt.ge 0)
  EVar _ :<: e | b == TInt -> Right $ e :-: EIntA (AInt.ge 1)

  EVar _ :=: e -> Right e

  EStrLen (EVar _) :=: EInt  n _ -> Right $ EStrA $ absStrLen (AInt.eq n)
  EStrLen (EVar _) :=: EIntA n   -> Right $ EStrA $ absStrLen n

  EStrAt (EVar _) (EInt  n _) :=: EChar c _                -> Right $ EStrA $ absStrAt (AInt.eq n) (AChar.eq c)
  EStrAt (EVar _) (EIntA n)   :=: EChar c _                -> Right $ EStrA $ absStrAt n           (AChar.eq c)
  EStrAt (EVar _) (EInt  n _) :=: EStrA (toChar -> Just c) -> Right $ EStrA $ absStrAt (AInt.eq n) c
  EStrAt (EVar _) (EIntA n)   :=: EStrA (toChar -> Just c) -> Right $ EStrA $ absStrAt n           c

  EStrAt (EVar s1) (EStrLen (EVar s2) :+: EInt  n _) :=: EStrA (toChar -> Just c) | x == s1, x == s2 -> Right $ EStrA $ absStrAtRev (AInt.sub (AInt.eq 0) (AInt.eq n)) c
  EStrAt (EVar s1) (EStrLen (EVar s2) :+: EIntA n)   :=: EStrA (toChar -> Just c) | x == s1, x == s2 -> Right $ EStrA $ absStrAtRev (AInt.sub (AInt.eq 0) n)           c
  EStrAt (EVar s1) (EStrLen (EVar s2) :-: EInt  n _) :=: EStrA (toChar -> Just c) | x == s1, x == s2 -> Right $ EStrA $ absStrAtRev (AInt.eq n)                        c
  EStrAt (EVar s1) (EStrLen (EVar s2) :-: EIntA n)   :=: EStrA (toChar -> Just c) | x == s1, x == s2 -> Right $ EStrA $ absStrAtRev n                                  c  

  EStrAt s (EVar _)       :=: c | x ∉ s        -> Right $ EFun "_StrAt_index" [s,c]
  EStrAt s (EVar _ :+: y) :=: c | x ∉ s, x ∉ y -> Right $ EFun "_StrAt_index" [s,c] :-: y
  EStrAt s (EVar _ :-: y) :=: c | x ∉ s, x ∉ y -> Right $ EFun "_StrAt_index" [s,c] :+: y

  EStrSub s i (EVar _)       :=: t | x ∉ s, x ∉ i        -> Right $ EFun "_StrSub_index_end" [s,t,i]  
  EStrSub s i (EVar _ :+: y) :=: t | x ∉ s, x ∉ i, x ∉ y -> Right $ EFun "_StrSub_index_end" [s,t,i] :-: y
  EStrSub s i (EVar _ :-: y) :=: t | x ∉ s, x ∉ i, x ∉ y -> Right $ EFun "_StrSub_index_end" [s,t,i] :+: y

  EStrSub s i (EVar _)       :≠: t | x ∉ s, x ∉ i        -> Right $ EFun "_StrSub_index_end" [s,EFun "_StrComp" [t],i]  
  EStrSub s i (EVar _ :+: y) :≠: t | x ∉ s, x ∉ i, x ∉ y -> Right $ EFun "_StrSub_index_end" [s,EFun "_StrComp" [t],i] :-: y
  EStrSub s i (EVar _ :-: y) :≠: t | x ∉ s, x ∉ i, x ∉ y -> Right $ EFun "_StrSub_index_end" [s,EFun "_StrComp" [t],i] :+: y


  EStrSub (EVar _) (EInt 0 _) (EIntA j) :≠: EStr t _
    | (j ∧ AInt.ge 0) == AInt.ge 0 
    -> Right $ EStrA $ neg $ AString.eq (Text.unpack t) <> star anyChar

  EStrSub (EVar _) (EInt 0 _) (EIntA j) :≠: EStrA t
    | (j ∧ AInt.ge 0) == AInt.ge 0 
    -> Right $ EStrA $ neg $ t <> star anyChar

  r' -> Left r'

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
  | isBot c = case AInt.minimum n of
     Just (Fin a) -> joins $ map (rep anyChar) [0..a]
     _            -> impossible
  | otherwise = joins $ flip concatMap (AInt.intervals n) $ \case
      AInt.In (Fin a) (Fin b) -> [rep anyChar i <> lit c <> star anyChar | i <- [a..b]]
      AInt.In (Fin a) PosInf  -> [rep anyChar a <> star anyChar <> lit c <> star anyChar]
      _                       -> impossible

absStrAtRev :: AInt -> AChar -> AString
absStrAtRev (meet (AInt.ge 1) -> n) c
  | isBot n   = bot
  | isBot c   = bot
  | otherwise = joins $ flip concatMap (AInt.intervals n) $ \case
      AInt.In (Fin a) (Fin b) -> [star anyChar <> lit c <> rep anyChar (i-1) | i <- [a..b]]
      AInt.In (Fin _) PosInf  -> [star anyChar <> lit c <> star anyChar]
      _                       -> impossible


-- | Independently normalize each side of a relation.
normRel :: Rel -> Rel
normRel (Rel op e1 e2) = Rel op (norm e1) (norm e2)
