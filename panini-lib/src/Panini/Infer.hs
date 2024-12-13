module Panini.Infer where

import Algebra.Lattice
import Control.Monad.Trans.State.Strict
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Panini.Environment
import Panini.Error
import Panini.Monad
import Panini.Provenance
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude

-- TODO: update type rules in paper; in particular: template generation / hole
-- instantiation, which has gotten more complicated since the OOPSLA submission

------------------------------------------------------------------------------

withPV :: (Type,Con) -> PV -> (Type,Con)
withPV (t,c) pv = (setPV pv t, c)  -- TODO: Con provenance

-- | Mappings of variables to types (Γ)
type Context = Map Name Type

-- | Check a term against a type in the given context, returning a verification
-- condition that needs to be proven valid in order for the type check to be
-- considered successful. The given type could be user-provided and may contain
-- holes (?). The returned type may be more precise than the given type and will
-- not contain any holes but may contain κ variables. Proving the VC valid
-- should provide an assignment for any extant κ variable.
check :: Context -> Term -> Type -> Pan Error (Type, Con)
check g e t̃ = do
  (t,c) <- infer g e
  t̂ <- fresh g t̃
  ĉ <- sub t t̂
  return (t̂, c ∧ ĉ)

-- | Infer the most precise type of a term in the given context, plus a
-- verification condition that needs to be proven valid. Proving the VC should
-- provide an assignment for any extant κ variables present in the type.
infer :: Context -> Term -> Pan Error (Type, Con)
infer g = \case
  
  -- inf/var ----------------------------------------------
  Val (Var x) -> do
    t <- case Map.lookup x g of
      Just t  -> return $ self x t
      Nothing -> Map.lookup x <$> gets environment >>= \case
        Just Assumed {_type}        -> return _type
        Just Verified {_solvedType} -> return _solvedType
        _                           -> throwError $ UnknownVar x    
    return $ (t, CTrue) `withPV` getPV x
  
  -- inf/con ----------------------------------------------
  Val (Con c) -> do
    let v = dummyName
    let b = typeOfValue c
    let p = PRel $ EVar v :=: ECon c
    let t = TBase v b (Known p) (getPV c)
    return (t, CTrue)
  
  -- inf/app ----------------------------------------------
  App e x pv -> do
    (tₑ, cₑ) <- infer g e
    case tₑ of
      TBase _ _ _ _ -> throwError $ ExpectedFunType e tₑ
      TFun y t₁ t₂ _ -> do
        (tₓ, _) <- infer g (Val x)
        cₓ <- sub tₓ t₁
        let t = subst (atomToExpr x) y t₂
        let c = cₑ ∧ cₓ
        return $ (t, c) `withPV` pv
  
  -- inf/lam ----------------------------------------------
  Lam x t̃₁ e pv | x `elem` Map.keys g -> do
    let x' = freshName x (Map.keys g)
    let e' = subst (Var x') x e
    infer g $ Lam x' t̃₁ e' pv

  Lam x t̃₁ e pv -> do
    t̂₁ <- fresh g (shape t̃₁)
    (t₂, c₂) <- infer (Map.insert x t̂₁ g) e
    let t = TFun x t̂₁ t₂ pv
    let c = cImpl x t̂₁ c₂
    return (t, c)
  
  -- inf/let ----------------------------------------------
  Let x e₁ e₂ pv | x `elem` Map.keys g -> do
    let x' = freshName x (Map.keys g)
    let e₂' = subst (Var x') x e₂
    infer g $ Let x' e₁ e₂' pv

  Let x e₁ e₂ pv -> do
    (t₁, c₁) <- infer g e₁
    (t₂, c₂) <- infer (Map.insert x t₁ g) e₂
    t̂₂ <- fresh g (shape t₂)    
    ĉ₂ <- sub t₂ t̂₂
    let c = c₁ ∧ (cImpl x t₁ (c₂ ∧ ĉ₂))
    return $ (t̂₂, c) `withPV` pv

  -- inf/rec ----------------------------------------------
  Rec x t̃₁ e₁ e₂ pv | x `elem` Map.keys g -> do
    let x' = freshName x (Map.keys g)
    let e₁' = subst (Var x') x e₁
    let e₂' = subst (Var x') x e₂
    infer g $ Rec x' t̃₁ e₁' e₂' pv

  Rec x t̃₁ e₁ e₂ pv -> do
    t̂₁      <- fresh g t̃₁
    (t₁,c₁) <- infer (Map.insert x t̂₁ g) e₁
    ĉ₁      <- sub t₁ t̂₁
    (t₂,c₂) <- infer (Map.insert x t₁ g) e₂
    t̂₂      <- fresh g (shape t₂)
    ĉ₂      <- sub t₂ t̂₂
    let c    = (cImpl x t̂₁ (c₁ ∧ ĉ₁)) ∧ (cImpl x t₁ (c₂ ∧ ĉ₂))
    return   $ (t̂₂,c) `withPV` pv

  -- inf/if -----------------------------------------------
  If v e₁ e₂ pv -> do
    checkBool g v
    (t₁,c₁) <- infer g e₁
    t̂       <- fresh g (shape t₁)
    ĉ₁      <- sub t₁ t̂
    (t₂,c₂) <- infer g e₂
    ĉ₂      <- sub t₂ t̂
    let p₁   = PRel $ atomToExpr v :=: EBool True NoPV
    let p₂   = PRel $ atomToExpr v :=: EBool False NoPV
    let y    = freshName "y" (freeVars v <> freeVars c₁ <> freeVars c₂)
    let c    = (CAll y TUnit p₁ (c₁ ∧ ĉ₁)) ∧ (CAll y TUnit p₂ (c₂ ∧ ĉ₂))
    return   $ (t̂,c) `withPV` pv

atomToExpr :: Atom -> Expr
atomToExpr (Con c) = ECon c
atomToExpr (Var x) = EVar x

checkBool :: Context -> Atom -> Pan Error ()
checkBool g v = do
  (tb,_) <- infer g (Val v)
  _ <- sub tb (TBase dummyName TBool (Known PTrue) NoPV)
  return ()

-- | Selfification.
self :: Name -> Type -> Type
self x = \case
  TBase v b (Known p) pv ->
    let v' = if v == x then freshName v (freeVars p) else v
        p' = (subst (EVar v') x p) ∧ PRel (EVar v' :=: EVar x)
    in TBase v' b (Known p') pv  
  t -> t

-- | Hole instantiation (▷).
fresh :: Context -> Type -> Pan Error Type
fresh g0 = go (Map.toList g0)
  where
    -- ins/hole -------------------------------------------
    go g (TBase v b Unknown pv) = do
      let (xs,ts) = unzip [(x,t) | (x, TBase _ t _ _) <- g]
      κ <- freshK (b:ts) pv
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

freshK :: [Base] -> PV -> Pan Error KVar
freshK ts pv = do
  i <- gets kvarCount
  modify $ \s -> s { kvarCount = i + 1}
  return $ KVar i ts pv

-- | Returns the non-refined version of a type.
shape :: Type -> Type
shape (TBase v b _ pv) = TBase v b Unknown (Derived pv "shape")
shape (TFun x t1 t2 pv) = TFun x (shape t1) (shape t2) (Derived pv "shape")

-- | Subtyping (⩽).
sub :: Type -> Type -> Pan Error Con
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

  _ -> throwError $ InvalidSubtype (unrefined lhs) (unrefined rhs)

-- | Returns the simple version of a type by stripping all refinement
-- accoutrements; basically an extended version of the 'shape' function.
unrefined :: Type -> Type
unrefined (TBase _ b _ pv) = TBase dummyName b (Known PTrue) pv
unrefined (TFun _ s t pv) = TFun dummyName (unrefined s) (unrefined t) pv

-- | Generalized implication that drops binders with non-basic types.
cImpl :: Name -> Type -> Con -> Con
cImpl x t c = case t of
  TBase v b (Known p) _ -> CAll x b (subst (EVar x) v p) c
  _                     -> c
