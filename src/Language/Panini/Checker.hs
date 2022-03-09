module Language.Panini.Checker where

import Data.List
import Data.Map (Map)
import Data.Map qualified as Map

import Language.Panini.Substitution
import Language.Panini.Syntax

-- | Type checker monad.
type TC a = Either TypeError a

failWith :: TypeError -> TC a
failWith = Left

data TypeError
  = InvalidSubtypingBase Base Base
  | InvalidSubtyping Type Type
  | VarNotInScope Name
  | ExpectedFunType Expr Type
  deriving (Show, Read)

------------------------------------------------------------------------------
-- Contexts

newtype Ctx = Ctx (Map Name Type)

lookupCtx :: Name -> Ctx -> TC Type
lookupCtx x (Ctx m) = 
  maybe (failWith $ VarNotInScope x) return $ Map.lookup x m

------------------------------------------------------------------------------

{-| Type synthesis.

@

  prim(c) = t
---------------  SYN-PRIM
  g |- c => t


  g(x) = t  
------------  SYN-VAR
 g |- x => t


  g |- s |> t      g |- e <= t
--------------------------------  SYN-ANN
        g |- e : s => t


  g |- e => x:s -> t      g |- y <= s  
---------------------------------------  SYN-APP
           g |- e y => t[y/x]

@
-}
synth :: Ctx -> Expr -> TC (Con, Type)

-- [SYN-PRIM]
-- [SYN-VAR]
synth g (Val v) = (cTrue, ) <$> case v of
  U   -> return $ simpleType TUnit
  B _ -> return $ simpleType TBool
  I _ -> return $ simpleType TInt
  S _ -> return $ simpleType TString
  V x -> lookupCtx x g

-- [SYN-ANN]
synth g (Ann e s) = do
  t <- fresh g s
  c <- check g e t
  return (c, t)

-- [SYN-APP]
synth g (App e y) = do
  (c,f) <- synth g e
  case f of
    TFun x s t -> do
      c' <- check g (Val y) s
      return (CConj c c', subst y x t)    
    
    _ -> failWith (ExpectedFunType e f)


------------------------------------------------------------------------------

{-| Type checking.

@



@
-}
check :: Ctx -> Expr -> Type -> TC Con
check = undefined

------------------------------------------------------------------------------

{-| Hole Instantiation

@

----------------------  INS-CONC
  {v:b|p} |> {v:b|p}

@
-}
fresh :: Ctx -> Type -> TC Type

-- [INS-CONC]
fresh _ t@(TBase v b (Known p)) = return t

fresh _ _ = error "not implemented yet"

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

-- [SUB-BASE]
sub (TBase v1 b1 (Known p1)) (TBase v2 b2 (Known p2))
  | b1 == b2  = return $ CAll v1 b1 p1 (CPred $ subst (V v1) v2 p2)
  | otherwise = failWith $ InvalidSubtypingBase b1 b2

-- [SUB-FUN]
sub (TFun x1 s1 t1) (TFun x2 s2 t2) = do
  cI <- sub s2 s1
  let t1' = subst (V x1) x2 t1
  cO <- implCon x2 s2 <$> sub t1' t2
  return $ CConj cI cO

sub t1 t2 = failWith $ InvalidSubtyping t1 t2

-- | Implication constraint @(x :: t) => c@.
implCon :: Name -> Type -> Con -> Con
implCon x t c = case t of
  TBase v b (Known p) -> CAll x b (subst (V x) v p) c
  _                   -> c
