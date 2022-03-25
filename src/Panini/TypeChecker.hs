{-# LANGUAGE OverloadedStrings #-}

module Panini.TypeChecker where

import Data.Map (Map)
import Data.Map qualified as Map
import Panini.Error
import Panini.Provenance
import Panini.Substitution
import Panini.Syntax
import Prelude

------------------------------------------------------------------------------

-- | Type checker monad.
type TC a = Either Error a

failWith :: Error -> TC a
failWith = Left

-- | Typing context (Gamma)
type Ctx = Map Name Type

------------------------------------------------------------------------------
{-| Type synthesis.

@

  prim(c) = t
---------------  SYN-PRIM
  g |- c => t


  g(x) = t  
------------  SYN-VAR
 g |- x => t


         g(x) = {v:b|p}  
--------------------------------  SYN-SELF
 g |- x => {v:b | p /\ v = x }


  g |- s |> t      g |- e <= t
--------------------------------  SYN-ANN
        g |- e : s => t


  g |- e => x:s -> t      g |- y <= s  
---------------------------------------  SYN-APP
           g |- e y => t[y/x]

@
-}
synth :: Ctx -> Term -> TC (Con, Type)

-- [SYN-PRIM]
synth _ (Val (U   pv)) = return (cTrue, simpleType TUnit   (Derived pv "SYN-PRIM"))
synth _ (Val (B _ pv)) = return (cTrue, simpleType TBool   (Derived pv "SYN-PRIM"))
synth _ (Val (I _ pv)) = return (cTrue, simpleType TInt    (Derived pv "SYN-PRIM"))
synth _ (Val (S _ pv)) = return (cTrue, simpleType TString (Derived pv "SYN-PRIM"))

-- [SYN-VAR]
-- [SYN-SELF]
synth g (Val (V x)) = do
  case Map.lookup x g of
    Just (TBase v b (Known p) pv) -> do
      let r' = Known (PConj p (PRel Eq (pVar v) (pVar x)))
      let t' = TBase v b r' (Derived pv "SYN-SELF")
      return (cTrue, t')    
    Just t -> return (cTrue, t)
    Nothing -> failWith $ VarNotInScope x
     
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
      return (c `cAnd` c', subst y x t)

    _ -> failWith $ ExpectedFunType e t0

synth _ e = failWith $ CantSynth e

------------------------------------------------------------------------------
{-| Type checking.

@

  g |- e => s      g |- s <: t
--------------------------------  CHK-SYN
          g |- e <= t


  g, x:t1 |- e <= t2[x/y]
---------------------------  CHK-LAM
  g |- \x.e <= y:t1 -> t2


  g |- e1 => t1      g, x:t1 |- e2 <= t2
------------------------------------------  CHK-LET
       g |- let x = e1 in e2 <= t2


  g |- s1 |> t1      g, x:t1 |- e1 <= t1      g, x:t1 |- e2 <= t2
-------------------------------------------------------------------  CHK-REC
                g |- rec x : s1 = e1 in e2 <= t2


  g |- x <= bool
  y is fresh
  g, y:{_:int |  x } |- e1 <= t
  g, y:{_:int | ~x } |- e2 <= t
----------------------------------  CHK-IF
  g |- if x then e1 else e2 <= t


@
-}
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
  return $ c1 `cAnd` (cImpl x t1 c2)

-- [CHK-REC]
check g (Rec x s1 e1 e2) t2 = do
  t1 <- fresh g s1
  let g' = Map.insert x t1 g
  c1 <- check g' e1 t1
  c2 <- check g' e2 t2
  return $ c1 `cAnd` c2

-- [CHK-IF]
check g (If x e1 e2) t = do
  _ <- check g (Val x) (simpleType TBool NoPV)
  c1 <- check g e1 t
  c2 <- check g e2 t
  let y = freshName "y" (freeVars x ++ freeVars c1 ++ freeVars c2)
  let yT = TBase dummyName TUnit (Known $ PVal x) NoPV
  let yF = TBase dummyName TUnit (Known $ PNot $ PVal x) NoPV
  return $ (cImpl y yT c1) `cAnd` (cImpl y yF c2)

-- [CHK-SYN]
check g e t = do
  (c, s) <- synth g e
  c' <- sub s t
  return $ c `cAnd` c'

------------------------------------------------------------------------------
{-| Hole Instantiation

@

------------------------------------- INS-HOLE
  xs:ts |- {v:b|?} |> {v:b|k(v,xs)}


----------------------  INS-CONC
  {v:b|p} |> {v:b|p}


  g |- s1 |> s2      g, x:s1 |- t1 |> t2
------------------------------------------  INS-FUN
      g |- x:s1 -> t1 |> x:s2 -> t2

@
-}
fresh :: Ctx -> Type -> TC Type

-- [INS-HOLE]
fresh g (TBase v b Unknown pv) = do
  let xs = Map.keys g
  let p = PHorn (Name "k0" NoPV) (map V (v:xs))  -- TODO: fresh horn var
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
{- | Subtyping.

@

  g |- forall v1:b. p1 => p2[v1/v2]
-------------------------------------  SUB-BASE
     g |- {v1:b|p1} <: {v2:b|p2}


  g |- s2 <: s1      g, x2:s2 |- t1[x2/x1] <: t2
--------------------------------------------------  SUB-FUN
          g |- x1:s1 -> t1 <: x2:s2 -> t2

@
-}
sub :: Type -> Type -> TC Con

sub t1@(TBase _ b1 _ _) t2@(TBase _ b2 _ _) | b1 /= b2 = 
  failWith $ InvalidSubtypeBase (t1,b1) (t2,b2)

-- [SUB-BASE]
sub (TBase v1 b (Known p1) _) (TBase v2 _ (Known p2) _) = do
  let p2' = CPred $ subst (V v1) v2 p2
  return $ CAll v1 b p1 p2'

-- [SUB-FUN]
sub (TFun x1 s1 t1 _) (TFun x2 s2 t2 _) = do
  cI <- sub s2 s1
  let t1' = subst (V x2) x1 t1
  cO <- sub t1' t2
  return $ cI `cAnd` cImpl x2 s2 cO

sub t1 t2 = failWith $ InvalidSubtype t1 t2

-- | Implication constraint @(x :: t) => c@.
cImpl :: Name -> Type -> Con -> Con
cImpl x t c = case t of
  TBase v b (Known p) _ -> CAll x b (subst (V x) v p) c
  _                     -> c
