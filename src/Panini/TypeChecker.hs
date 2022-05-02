{-# LANGUAGE OverloadedStrings #-}

module Panini.TypeChecker where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Panini.Error
import Panini.Provenance
import Panini.Substitution
import Panini.Syntax
import Prelude

------------------------------------------------------------------------------

-- | Type checker monad.
type TC a = StateT TCState (Either Error) a

data TCState = TCState
  { tcHornCount :: !Int
  }

runTC :: TC a -> Either Error a
runTC m = do
  let s0 = TCState 0
  (x,_) <- runStateT m s0
  return x

failWith :: Error -> TC a
failWith = lift . Left

-- | Typing context (Gamma)
type Ctx = Map Name Type

------------------------------------------------------------------------------

-- | Synthesize type of term (↗).
synth :: Ctx -> Term -> TC (Pred, Type)

-- [SYN-VAR]
-- [SYN-SELF]
synth g (Val (V x)) = do
  case Map.lookup x g of
    Just (TBase v b (Known p) pv) -> do
      let v' = if v == x then freshName v (freeVars p) else v
          p' = subst (V v') v p
          r' = Known (p' `pAnd` (pVar v' `pEq` pVar x))
          t' = TBase v' b r' (Derived pv "SYN-SELF")
      return (pTrue, t')    
    Just t -> return (pTrue, t)
    Nothing -> failWith $ VarNotInScope x

-- [SYN-PRIM]
synth _ (Val c) = case c of
  U   _ -> return (pTrue, primType TUnit)
  B _ _ -> return (pTrue, primType TBool)
  I _ _ -> return (pTrue, primType TInt)
  S _ _ -> return (pTrue, primType TString)
  where
    primType b = TBase v b (Known (pVar v `pEq` PVal c)) pv
    v = dummyName
    pv = Derived (getPV c) "SYN-PRIM"

-- [SYN-ANN]
synth g (Ann e s) = do
  t <- fresh g s
  c <- check g e t
  return (c, t)

-- [SYN-APP]
synth g (App e y) = do
  (c,t0) <- synth g e
  case t0 of
    TFun x s t _ -> do
      c' <- check g (Val y) s
      return (c `pAnd` c', subst y x t)

    _ -> failWith $ ExpectedFunType e t0

synth _ e = failWith $ CantSynth e

------------------------------------------------------------------------------

-- | Check type of term (↙).
check :: Ctx -> Term -> Type -> TC Pred

-- [CHK-LAM]
check g (Lam x e) (TFun y s t _) = do
  let g' = Map.insert x s g
      t' = subst (V x) y t
  c <- check g' e t'
  return $ cImpl x s c

check _ (Lam x e) t = failWith $ ExpectedFunType (Lam x e) t

-- [CHK-LET]
check g (Let x e1 e2) t2 = do
  (c1, t1) <- synth g e1
  let g' = Map.insert x t1 g
  c2 <- check g' e2 t2
  return $ c1 `pAnd` (cImpl x t1 c2)

-- [CHK-REC]
check g (Rec x s1 e1 e2) t2 = do
  t1 <- fresh g s1
  let g' = Map.insert x t1 g
  c1 <- check g' e1 t1
  c2 <- check g' e2 t2
  return $ c1 `pAnd` c2

-- [CHK-IF]
check g (If x e1 e2) t = do
  _ <- check g (Val x) (TBase dummyName TBool (Known pTrue) NoPV)
  c1 <- check g e1 t
  c2 <- check g e2 t
  return $ (PImpl (PVal x) c1) `pAnd` (PImpl (PNot (PVal x)) c2)

-- [CHK-SYN]
check g e t = do
  (c, s) <- synth g e
  c' <- sub s t
  return $ c `pAnd` c'

------------------------------------------------------------------------------

-- | Hole instantiation (▷).
fresh :: Ctx -> Type -> TC Type

-- [INS-HOLE]
fresh g (TBase v b Unknown pv) = do
  i <- gets tcHornCount
  modify' (\s -> s { tcHornCount = s.tcHornCount + 1})
  let k = Text.pack $ "k" ++ show i
  let xs = map fst $ filter (isBaseType . snd) $ Map.toList g
  let p = PHorn (Name k NoPV) (map V (v:xs))
  return $ TBase v b (Known p) (Derived pv "INS-HOLE")

-- [INS-CONC]
fresh _ t@(TBase _ _ (Known _) _) = return t

-- [INS-FUN]
fresh g (TFun x s1 t1 pv) = do
  s2 <- fresh g s1
  let g' = Map.insert x s1 g
  t2 <- fresh g' t1
  return $ TFun x s2 t2 (Derived pv "INS-FUN")

------------------------------------------------------------------------------

-- | Subtyping (<:).
sub :: Type -> Type -> TC Pred

-- [SUB-BASE]
sub t1@(TBase v1 b1 (Known p1) _) t2@(TBase v2 b2 (Known p2) _)
  | b1 == b2  = let p2' = subst (V v1) v2 p2
                in return $ PAll v1 b1 $ PImpl p1 p2'
  | otherwise = failWith $ InvalidSubtypeBase (t1,b1) (t2,b2)

-- [SUB-FUN]
sub (TFun x1 s1 t1 _) (TFun x2 s2 t2 _) = do
  cI <- sub s2 s1
  let t1' = subst (V x2) x1 t1
  cO <- sub t1' t2
  return $ cI `pAnd` cImpl x2 s2 cO

sub t1 t2 = failWith $ InvalidSubtype t1 t2

------------------------------------------------------------------------------

-- | Implication constraint @(x :: t) => c@.
cImpl :: Name -> Type -> Pred -> Pred
cImpl x t c = case t of
  TBase v b (Known p) _ -> PAll x b $ PImpl (subst (V x) v p) c
  _                     -> c
