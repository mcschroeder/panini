module Panini.Infer where

import Algebra.Lattice
import Control.Monad.Trans.State.Strict
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Panini.Error
import Panini.Monad
import Panini.Panic
import Panini.Provenance
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude

-- TODO: update type rules in paper

------------------------------------------------------------------------------

withPV :: (Type,Con) -> PV -> (Type,Con)
withPV (t,c) pv = (setPV pv t, c)  -- TODO: Con provenance

-- | Mappings of variables to types (Γ)
type Context = Map Name Type

-- | Infer the most precise type of a term in the given context, plus a
-- verification condition that needs to be proven valid. Proving the VC might
-- should provide an assignment for any extant κ variables present in the type.
infer :: Context -> Term -> Pan (Type, Con)
infer g = \case
  
  -- inf/var ----------------------------------------------
  Val (Var x) -> do
    case Map.lookup x g of
      Nothing -> throwError $ VarNotInScope x
      Just t -> return $ (self x t, CTrue) `withPV` getPV x
  
  -- inf/con ----------------------------------------------
  Val (Con c) -> do
    let v = dummyName
    let b = primType c
    let t = TBase v b (Known (EVar v `pEq` ECon c)) (getPV c)
    return (t, CTrue)
    where
      primType (U   _) = TUnit
      primType (B _ _) = TBool
      primType (I _ _) = TInt
      primType (S _ _) = TString
  
  -- inf/app ----------------------------------------------
  App e x pv -> do
    (tₑ, cₑ) <- infer g e
    case tₑ of
      TBase _ _ _ _ -> throwError $ ExpectedFunType e tₑ
      TFun y t₁ t₂ _ -> do
        (tₓ, _) <- infer g (Val x)
        cₓ <- sub tₓ t₁
        let t = subst (EVal x) y t₂
        let c = cₑ ∧ cₓ
        return $ (t, c) `withPV` pv
  
  -- inf/lam ----------------------------------------------
  Lam x t̃₁ e pv -> do
    t̂₁ <- fresh (shape t̃₁)
    (t₂, c₂) <- infer (Map.insert x t̂₁ g) e
    let t = TFun x t̂₁ t₂ pv
    let c = cImpl x t̂₁ c₂
    return (t, c)
  
  -- inf/let ----------------------------------------------
  Let x e₁ e₂ pv -> do
    (t₁, c₁) <- infer g e₁
    (t₂, c₂) <- infer (Map.insert x t₁ g) e₂
    t̂₂ <- fresh (shape t₂)
    ĉ₂ <- sub t₂ t̂₂
    let c = c₁ ∧ (cImpl x t₁ (c₂ ∧ ĉ₂))
    return $ (t̂₂, c) `withPV` pv

  -- inf/rec ----------------------------------------------
  Rec x t̃₁ e₁ e₂ pv -> do
    t̂₁ <- fresh (shape t̃₁)
    (t₁, c₁) <- infer (Map.insert x t̂₁ g) e₁
    (t₂, c₂) <- infer (Map.insert x t₁ g) e₂
    t̂₂ <- fresh (shape t₂)
    ĉ₂ <- sub t₂ t̂₂
    let c = (cImpl x t̂₁ c₁) ∧ (cImpl x t₁ c₂) ∧ ĉ₂
    return $ (t₂, c) `withPV` pv

  -- inf/if -----------------------------------------------
  If v e₁ e₂ pv -> do
    checkBool g v
    (t₁, c₁) <- infer g e₁
    (t₂, c₂) <- infer g e₂
    let y = freshName "y" (freeVars v <> freeVars c₁ <> freeVars c₂)
    let p₁ = EVal v `pEq` ECon (B True  NoPV)
    let p₂ = EVal v `pEq` ECon (B False NoPV)
    let c = (CAll y TUnit p₁ c₁) ∧ (CAll y TUnit p₂ c₂)
    t <- mkJoin t₁ t₂
    return $ (t, c) `withPV` pv

checkBool :: Context -> Value -> Pan ()
checkBool g v = do
  (tb,_) <- infer g (Val v)
  _ <- sub tb (TBase dummyName TBool (Known PTrue) NoPV)
  return ()

-- | Selfification.
self :: Name -> Type -> Type
self x = \case
  TBase v b (Known p) pv ->
    let v' = if v == x then freshName v (freeVars p) else v
        p' = (subst (EVar v') x p) ∧ (EVar v' `pEq` EVar x)
    in TBase v' b (Known p') pv  
  t -> t

-- | Hole instantiation (▷).
fresh :: Type -> Pan Type  -- TODO: replace Infer monad with source constraint?
fresh = go []
  where
    -- ins/hole -------------------------------------------
    go g (TBase v b Unknown pv) = do
      let (xs,ts) = unzip [(x,t) | (x, TBase _ t _ _) <- g]
      κ <- freshK (b:ts)
      let v' = if v `elem` xs then freshName v xs else v
      let p = PAppK κ (map EVar (v':xs))
      return $ TBase v' b (Known p) (Derived pv "ins/hole")
    
    -- ins/conc -------------------------------------------
    go _ t@(TBase _ _ (Known _) _) = return t

    -- ins/fun --------------------------------------------
    go g (TFun x s t pv) = do
      ŝ <- go g s
      t̂ <- go ((x,s):g) t
      return $ TFun x ŝ t̂ (Derived pv "ins/fun")

freshK :: [Base] -> Pan KVar
freshK ts = do
  i <- gets kvarCount
  modify $ \s -> s { kvarCount = i + 1}
  return $ KVar i ts

-- | Returns the non-refined version of a type.
shape :: Type -> Type
shape (TBase v b _ pv) = TBase v b Unknown pv
shape (TFun x t1 t2 pv) = TFun x (shape t1) (shape t2) pv

-- | Subtyping (⩽).
sub :: Type -> Type -> Pan Con
sub lhs rhs = case (lhs, rhs) of
  
  -- sub/base ---------------------------------------------
  (TBase v₁ b₁ (Known p₁) _, TBase v₂ b₂ (Known p₂) _)
    | b₁ == b₂ -> return $ CAll v₁' b₁ p₁' (CHead p₂')
      where
        fvs = freeVars lhs <> freeVars rhs
        v₁' = if v₁ `elem` fvs then freshName v₁ fvs else v₁
        p₁' = subst (EVar v₁') v₁ p₁
        p₂' = subst (EVar v₁') v₂ p₂

  -- sub/fun ----------------------------------------------
  (TFun x₁ s₁ t₁ _, TFun x₂ s₂ t₂ _) -> do
    cᵢ <- sub s₂ s₁
    cₒ <- sub (subst (EVar x₂) x₁ t₁) t₂
    return $ cᵢ ∧ (cImpl x₂ s₂ cₒ)

  _ -> throwError $ InvalidSubtype lhs rhs


-- | Generalized implication that drops binders with non-basic types.
cImpl :: Name -> Type -> Con -> Con
cImpl x t c = case t of
  TBase v b (Known p) _ -> CAll x b (subst (EVar x) v p) c
  _                     -> c

-- | The join (⊔) of two types.
mkJoin :: Type -> Type -> Pan Type
mkJoin t₁ t₂ = case (t₁, t₂) of
  -- join/base --------------------------------------------
  (TBase v₁ b₁ (Known p₁) _, TBase v₂ b₂ (Known p₂) _)
    | b₁ == b₂ -> 
        let p = p₁ ∨ subst (EVar v₁) v₂ p₂
        in return $ TBase v₁ b₁ (Known p) NoPV -- TODO: join provenance
  
  _ -> panic "invalid join"  -- TODO: correct error
