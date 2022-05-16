{-# LANGUAGE OverloadedStrings #-}

module Panini.TypeChecker where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Panini.Error
import Panini.Printer
import Panini.Provenance
import Panini.Substitution
import Panini.Syntax
import Prelude
import Debug.Trace

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

self :: Ctx -> Name -> TC Type
self g x = case Map.lookup x g of
  Just (TBase v b (Known p) pv) -> do
    let v' = if v == x then freshName v (freeVars p) else v
    let p' = (subst (V v') x p) `pAnd` (pVar v' `pEq` pVar x)
    return (TBase v' b (Known p') pv)
  Just t -> return t
  Nothing -> failWith $ VarNotInScope x

-- | Synthesize type of term (↗).
synth :: Ctx -> Term -> TC (Con, Type)

-- [SYN-VAR]
-- [SYN-SELF]
synth g e0@(Val (V x)) = do
  t <- self g x
  let vc = CTrue (getPV t) 
  traceM $ showPretty e0 ++ " : " ++ showPretty t ++ " ⫤ " ++ showPretty vc
  return (vc, t)

  -- case Map.lookup x g of
  --   Just (TBase v b (Known p) pv) -> do
  --     let v' = if v == x then freshName v (freeVars p) else v
  --         p' = subst (V v') v p
  --         r' = Known (p' `pAnd` (pVar v' `pEq` pVar x))
  --         pv' = Derived pv "SYN-SELF"
  --         t' = TBase v' b r' pv'
  --     return (CTrue pv', t')
  --   Just t -> return (CTrue (getPV t), t)
  --   Nothing -> failWith $ VarNotInScope x

-- [SYN-PRIM]
synth _ (Val c) = case c of
  U   _ -> debug_return (CTrue pv, primType TUnit)
  B _ _ -> debug_return (CTrue pv, primType TBool)
  I _ _ -> debug_return (CTrue pv, primType TInt)
  S _ _ -> debug_return (CTrue pv, primType TString)
  where
    primType b = TBase v b (Known (pVar v `pEq` PVal c)) pv
    v = dummyName
    pv = Derived (getPV c) "SYN-PRIM"

    debug_return (vc, t) = do
      traceM (showPretty (Val c) ++ " : " ++ showPretty t ++ " ⫤ " ++ showPretty vc)
      return (vc,t)

-- [SYN-ANN]
-- synth g (Ann e s) = do
--   t <- fresh g s
--   c <- check g e t
--   return (c, t)

-- [SYN-APP]
-- synth g (App e y) = do
--   (c,t0) <- synth g e
--   case t0 of
--     TFun x s t _ -> do
--       c' <- check g (Val y) s
--       return (c `cAnd` c', subst y x t)

--     _ -> failWith $ ExpectedFunType e t0

-----
synth g e0@(Let x e1 e2) = do
  (c1, t1) <- synth g e1
  (c2, t2) <- synth (Map.insert x t1 g) e2
  t2' <- fresh mempty (shape t2)
  c3 <- sub t2 t2'
  let vc = c1 `cAnd` (cImpl x t1 (c2 `cAnd` c3)) 
  --traceM $ showPretty e0 ++ " : " ++ showPretty t2' ++ " ⫤ " ++ showPretty vc
  return (vc, t2')

synth g e0@(Lam2 x t1 e) = do
  t1' <- fresh mempty (shape t1)
  (c, t2) <- synth (Map.insert x t1' g) e
  let t'' = TFun x t1' t2 NoPV
  let vc = cImpl x t1' c
  --traceM $ showPretty e0 ++ " : " ++ showPretty t'' ++ " ⫤ " ++ showPretty vc
  return (vc, t'')

synth g e0@(App e (V y)) = do
  (c,t0) <- synth g e
  case t0 of
    TFun x s t _ -> do
      s' <- self g y
      cy <- sub s' s
      let t' = subst (V y) x t
      let vc = c `cAnd` cy
      traceM $ showPretty e0 ++ " : " ++ showPretty t' ++ " ⫤ " ++ showPretty vc
      return (vc, t')

    _ -> failWith $ ExpectedFunType e t0

-----


synth _ e = failWith $ CantSynth e


shape :: Type -> Type
shape (TBase v b _ pv) = TBase v b Unknown pv
shape (TFun x t1 t2 pv) = TFun x (shape t1) (shape t2) pv

------------------------------------------------------------------------------

-- | Check type of term (↙).
check :: Ctx -> Term -> Type -> TC Con

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
  return $ c1 `cAnd` cImpl x t1 c2

-- [CHK-REC]
check g (Rec x s1 e1 e2) t2 = do
  t1 <- fresh g s1
  let g' = Map.insert x t1 g
  c1 <- check g' e1 t1
  c2 <- check g' e2 t2
  return $ c1 `cAnd` c2

-- [CHK-IF]
check g (If x e1 e2) t = do
  _ <- check g (Val x) (TBase dummyName TBool (Known (PTrue NoPV)) NoPV)
  c1 <- check g e1 t
  c2 <- check g e2 t
  let y = freshName "y" (freeVars c1 ++ freeVars c2 ++ freeVars x)
  return $ (CAll y TUnit (PVal x) c1) `cAnd` (CAll y TUnit (PNot (PVal x)) c2)

-- [CHK-SYN]
check g e t = do
  (c, s) <- synth g e
  c' <- sub s t
  return $ c `cAnd` c'

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
sub :: Type -> Type -> TC Con

-- [SUB-BASE]
sub t1@(TBase v1 b1 (Known p1) _) t2@(TBase v2 b2 (Known p2) _)
  | b1 == b2  = let p2' = subst (V v1) v2 p2
                in return $ CAll v1 b1 p1 (CHead p2')
  | otherwise = failWith $ InvalidSubtypeBase (t1,b1) (t2,b2)

-- [SUB-FUN]
sub (TFun x1 s1 t1 _) (TFun x2 s2 t2 _) = do
  cI <- sub s2 s1
  let t1' = subst (V x2) x1 t1
  cO <- sub t1' t2
  return $ cI `cAnd` cImpl x2 s2 cO

sub t1 t2 = failWith $ InvalidSubtype t1 t2

------------------------------------------------------------------------------

-- | Implication constraint @(x :: t) => c@.
cImpl :: Name -> Type -> Con -> Con
cImpl x t c = case t of
  TBase v b (Known p) _ -> CAll x b (subst (V x) v p) c
  _                     -> c
