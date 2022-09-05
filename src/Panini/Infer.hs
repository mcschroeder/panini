module Panini.Infer where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Panini.Error
import Panini.Syntax
import Prelude

------------------------------------------------------------------------------

-- | Type inference monad.
type Infer a = StateT InferState (Either Error) a

-- | Type inference state.
data InferState = InferState 
  { kvarCount :: !Int  -- ^ source of fresh Horn variable names
  }

runInfer :: Infer a -> Either Error a
runInfer m = do
  let s0 = InferState { kvarCount = 0 }
  (x,_) <- runStateT m s0
  return x

failWith :: Error -> Infer a
failWith = lift . Left

freshK :: [Base] -> Infer KVar
freshK ts = do
  i <- gets kvarCount
  modify $ \s -> s { kvarCount = i + 1}
  return $ KVar i ts

------------------------------------------------------------------------------

-- | Mappings of variables to types (Γ)
type Context = Map Name Type

withType :: ((Type, Con) -> Term Typed) -> (Type, Con) -> (Term Typed, Type, Con)
withType f (t,vc) = (f (t,vc), t, vc)

infer :: Context -> Term Untyped -> Infer (Term Typed, Type, Con)
infer g = \case
  
  -- inf/var ----------------------------------------------
  Val (Var x) _ -> do
    case Map.lookup x g of
      Nothing -> failWith $ VarNotInScope x
      Just t -> return $ Val (Var x) `withType` (self x t, CTrue)
  
  -- inf/con ----------------------------------------------
  Val (Con c) _ -> do
    let v = dummyName
    let b = primType c
    let t = TBase v b (Known (PVar v `pEq` PCon c)) (getPV c)
    return $ Val (Con c) `withType` (t, CTrue)
    where
      primType (U   _) = TUnit
      primType (B _ _) = TBool
      primType (I _ _) = TInt
      primType (S _ _) = TString
  
  -- inf/app ----------------------------------------------
  App e v pv _ -> do
    (ė, tₑ, cₑ) <- infer g e
    case tₑ of
      TBase _ _ _ _ -> failWith undefined  --  $ ExpectedFunType ė t
      TFun y t₁ t₂ _ -> do
        (_, tᵥ, _) <- infer g (Val v ())
        cᵥ <- sub tᵥ t₁
        let t = subst v y t₂
        let c = cₑ ∧ cᵥ
        return $ App ė v pv `withType` (t, c)
  
  -- inf/lam ----------------------------------------------
  Lam x t̃₁ e pv _ -> do
    t̂₁ <- fresh (shape t̃₁)
    ĉ₁ <- sub t̃₁ t̂₁
    (ė, t₂, c₂) <- infer (Map.insert x t̂₁ g) e
    let t = TFun x t̂₁ t₂ NoPV
    let c = ĉ₁ ∧ (cImpl x t̂₁ c₂)
    return $ Lam x t̃₁ ė pv `withType` (t, c)
  
  -- inf/let ----------------------------------------------
  Let x e₁ e₂ pv _ -> do
    (ė₁, t₁, c₁) <- infer g e₁
    (ė₂, t₂, c₂) <- infer (Map.insert x t₁ g) e₂
    t̂₂ <- fresh (shape t₂)
    ĉ₂ <- sub t₂ t̂₂
    let c = c₁ ∧ (cImpl x t₁ c₂) ∧ ĉ₂
    return $ Let x ė₁ ė₂ pv `withType` (t̂₂, c)

  -- inf/rec ----------------------------------------------
  Rec x t̃₁ e₁ e₂ pv _ -> do
    t̂₁ <- fresh (shape t̃₁)
    (ė₁, t₁, c₁) <- infer (Map.insert x t̂₁ g) e₁
    (ė₂, t₂, c₂) <- infer (Map.insert x t₁ g) e₂
    t̂₂ <- fresh (shape t₂)
    ĉ₂ <- sub t₂ t̂₂
    let c = (cImpl x t̂₁ c₁) ∧ (cImpl x t₁ c₂) ∧ ĉ₂
    return $ Rec x t̃₁ ė₁ ė₂ pv `withType` (t₂, c)

  -- inf/if -----------------------------------------------
  If v e₁ e₂ pv _ -> do
    -- TODO: check that v is bool
    (ė₁, t₁, c₁) <- infer g e₁
    (ė₂, t₂, c₂) <- infer g e₂
    let y = freshName "y" (freeVars v ++ freeVars c₁ ++ freeVars c₂)
    let p₁ = PVal v `pEq` PCon (B True  NoPV)
    let p₂ = PVal v `pEq` PCon (B False NoPV)
    let c = (CAll y TUnit p₁ c₁) ∧ (CAll y TUnit p₂ c₂)
    t <- join t₁ t₂
    return $ If v ė₁ ė₂ pv `withType` (t, c)


(∧) :: Con -> Con -> Con
(∧) = cAnd

-- | Selfification.
self :: Name -> Type -> Type
self x = \case
  TBase v b (Known p) pv ->
    let v' = if v == x then freshName v (freeVars p) else v
        p' = (subst (Var v') x p) `pAnd` (PVar v' `pEq` PVar x)
    in TBase v' b (Known p') pv  
  t -> t

-- | Hole instantiation (▷).
fresh :: Type -> Infer Type  -- TODO: replace Infer monad with source constraint?
fresh = go []
  where
    -- ins/hole -------------------------------------------
    go g (TBase v b Unknown pv) = do
      let (xs,ts) = unzip [(x,t) | (x, TBase _ t _ _) <- g]
      κ <- freshK (b:ts)
      let p = PAppK κ (Var v : xs)
      return $ TBase v b (Known p) (Derived pv "ins/hole")
    
    -- ins/conc -------------------------------------------
    go _ t@(TBase _ _ (Known _) _) = return t

    -- ins/fun --------------------------------------------
    go g (TFun x s t pv) = do
      ŝ <- go g s
      t̂ <- go ((Var x, s) : g) t
      return $ TFun x ŝ t̂ (Derived pv "ins/fun")
      
-- | Returns the non-refined version of a type.
shape :: Type -> Type
shape (TBase v b _ pv) = TBase v b Unknown pv
shape (TFun x t1 t2 pv) = TFun x (shape t1) (shape t2) pv

-- | Subtyping (⩽).
sub :: Type -> Type -> Infer Con
sub lhs rhs = case (lhs, rhs) of
  
  -- sub/base ---------------------------------------------
  (TBase v₁ b₁ (Known p₁) _, TBase v₂ b₂ (Known p₂) _)
    | b₁ == b₂ -> return $ CAll v₁ b₁ p₁ $ CHead $ subst (Var v₁) v₂ p₂

  -- sub/fun ----------------------------------------------
  (TFun x₁ s₁ t₁ _, TFun x₂ s₂ t₂ _) -> do
    cᵢ <- sub s₂ s₁
    cₒ <- sub (subst (Var x₂) x₁ t₁) t₂
    return $ cᵢ ∧ (cImpl x₂ s₂ cₒ)

  _ -> failWith $ InvalidSubtype lhs rhs


-- | Generalized implication that drops binders with non-basic types.
cImpl :: Name -> Type -> Con -> Con
cImpl x t c = case t of
  TBase v b (Known p) _ -> CAll x b (subst (Var x) v p) c
  _                     -> c

-- | The join (⊔) of two types.
join :: Type -> Type -> Infer Type
join t₁ t₂ = case (t₁, t₂) of
  -- join/base --------------------------------------------
  (TBase v₁ b₁ (Known p₁) _, TBase v₂ b₂ (Known p₂) _)
    | b₁ == b₂ -> 
        let p = p₁ `pOr` subst (Var v₁) v₂ p₂
        in return $ TBase v₁ b₁ (Known p) NoPV -- TODO: join provenance
  
  _ -> error "invalid join"  -- TODO: correct error
