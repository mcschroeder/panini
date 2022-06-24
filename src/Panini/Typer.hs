{-# LANGUAGE OverloadedStrings #-}

module Panini.Typer where

import Control.Monad.Trans.State.Strict
import Data.Map (Map)
import Data.Map qualified as Map
import Panini.Error
import Panini.Syntax
import Prelude
import Data.Functor
import Control.Monad.Trans.Class

------------------------------------------------------------------------------

-- TODO: consider renaming the monad and this module to "Infer"

-- | The 'T' monad is where we do all our typing.
type T a = StateT TS (Either Error) a

-- | The state for the 'T' monad.
data TS = TS 
  { context :: Context
  , hornVarCount :: !Int
  }

runT :: T a -> Either Error a
runT m = do
  let s0 = TS { context = mempty, hornVarCount = 0 }
  (x,_) <- runStateT m s0
  return x

failWith :: Error -> T a
failWith = lift . Left

------------------------------------------------------------------------------

type Context = Map Name Type

withExtendedContext :: Name -> Type -> T a -> T a
withExtendedContext = undefined

data Term2 a
  = Val2 Value                          PV a  -- x
  | App2 (Term2 a) Value                PV a  -- e x
  | Lam3 Name Type (Term2 a)            PV a  -- \x:t. e
  | Let2 Name (Term2 a) (Term2 a)       PV a  -- let x = e1 in e2
  | Rec2 Name Type (Term2 a) (Term2 a)  PV a  -- rec x:t = e1 in e2
  | If2 Value (Term2 a) (Term2 a)       PV a  -- if x then e1 else e2
  deriving stock (Show, Read, Functor)

type Untyped = ()
type Typed = (Type, Con)

typeOf :: Term2 Typed -> (Type, Con)
typeOf = \case
  Val2 _       _ t -> t
  App2 _ _     _ t -> t
  Lam3 _ _ _   _ t -> t
  Let2 _ _ _   _ t -> t
  Rec2 _ _ _ _ _ t -> t
  If2 _ _ _    _ t -> t

withType :: ((Type, Con) -> Term2 Typed) -> (Type, Con) -> (Term2 Typed, Type, Con)
withType f (t,vc) = (f (t,vc), t, vc)

infer :: Term2 Untyped -> T (Term2 Typed, Type, Con)
infer = \case  
  
  -- inf/var ----------------------------------------------
  Val2 (V x) pv _ -> do
    g <- gets context
    case Map.lookup x g of
      Nothing -> failWith $ VarNotInScope x
      Just t -> return $ Val2 (V x) pv `withType` (self x t, CTrue pv)
  
  -- inf/con ----------------------------------------------
  Val2 c pv _ -> do
    let v = dummyName
    let b = primType c
    let t = TBase v b (Known (pVar v `pEq` PVal c)) pv
    return $ Val2 c pv `withType` (t, CTrue pv)
    where
      primType (U   _) = TUnit
      primType (B _ _) = TBool
      primType (I _ _) = TInt
      primType (S _ _) = TString
      primType _       = undefined  -- TODO: define Value differently?
  
  -- inf/app ----------------------------------------------
  App2 e x pv _ -> do
    (ė, tₑ, cₑ) <- infer e
    case tₑ of
      TBase _ _ _ _ -> failWith undefined  --  $ ExpectedFunType ė t
      TFun y t₁ t₂ _ -> do
        (_,tₓ,_ ) <- infer $ Val2 x NoPV ()
        cₓ <- sub tₓ t₁
        let t = subst x y t₂
        let c = cₑ ∧ cₓ
        return $ App2 ė x pv `withType` (t, c)
  
  -- inf/lam ----------------------------------------------  
  Lam3 x t̃₁ e pv _ -> do
    t̂₁ <- fresh (shape t̃₁)
    ĉ₁ <- sub t̃₁ t̂₁
    (ė, t₂, c₂) <- with (x ↦ t̂₁) (infer e)
    let t = TFun x t̂₁ t₂ NoPV
    let c = ĉ₁ ∧ (cImpl x t̂₁ c₂)
    return $ Lam3 x t̃₁ ė pv `withType` (t, c)
  
  -- inf/let ----------------------------------------------  
  Let2 x e₁ e₂ pv _ -> do
    (ė₁, t₁, c₁) <- infer e₁
    (ė₂, t₂, c₂) <- with (x ↦ t₁) (infer e₂)
    t̂₂ <- fresh (shape t₂)
    ĉ₂ <- sub t₂ t̂₂
    let c = c₁ ∧ (cImpl x t₁ c₂) ∧ ĉ₂
    return $ Let2 x ė₁ ė₂ pv `withType` (t̂₂, c)

  -- inf/rec ----------------------------------------------  
  Rec2 x t̃₁ e₁ e₂ pv _ -> do
    t̂₁ <- fresh (shape t̃₁)
    (ė₁, t₁, c₁) <- with (x ↦ t̂₁) (infer e₁)
    (ė₂, t₂, c₂) <- with (x ↦ t₁) (infer e₂)
    t̂₂ <- fresh (shape t₂)
    ĉ₂ <- sub t₂ t̂₂
    let c = (cImpl x t̂₁ c₁) ∧ (cImpl x t₁ c₂) ∧ ĉ₂
    return $ Rec2 x t̃₁ ė₁ ė₂ pv `withType` (t₂, c)

  -- inf/if -----------------------------------------------
  If2 x e₁ e₂ pv _ -> do
    -- TODO: check that x is bool
    (ė₁, t₁, c₁) <- infer e₁
    (ė₂, t₂, c₂) <- infer e₂
    let y = freshName "y" (freeVars c₁ ++ freeVars c₂ ++ freeVars x)
    let c = (CAll y TUnit (PVal x) c₁) ∧ (CAll y TUnit (PNot (PVal x)) c₂)
    t <- join t₁ t₂
    return $ If2 x ė₁ ė₂ pv `withType` (t, c)

-- | @with f m@ runs action @m@ on a state modified by (stricty) applying @f@
-- and returns the result without modifying the outer computation's state.
with :: Monad m => (s -> s) -> StateT s m a -> StateT s m a
with f m = lift . evalStateT (modify' f >> m) =<< get

-- | Extends a typing state context with the given mapping.
(↦) :: Name -> Type -> TS -> TS
(↦) x t s = s { context = Map.insert x t s.context }

(∧) :: Con -> Con -> Con
(∧) = cAnd

self :: Name -> Type -> Type
self x = \case
  TBase v b (Known p) pv -> TBase v' b (Known p') pv
    where
      v' = if v == x then freshName v (freeVars p) else v
      p' = (subst (V v') x p) `pAnd` (pVar v' `pEq` pVar x)
  
  t -> t
  
fresh :: Type -> T Type
fresh = undefined

shape :: Type -> Type
shape = undefined

sub :: Type -> Type -> T Con
sub = undefined

cImpl :: Name -> Type -> Con -> Con
cImpl = undefined

join :: Type -> Type -> T Type
join = undefined
