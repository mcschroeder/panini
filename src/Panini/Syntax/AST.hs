{-# LANGUAGE OverloadedStrings #-}

-- TODO: module documentation
module Panini.Syntax.AST where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Data.Text (Text)
import Panini.Syntax.Names
import Panini.Syntax.Provenance
import Prelude

------------------------------------------------------------------------------

-- | A program is simply a list of successive statements.
type Program = [Statement]

-- | Statements are top-level declarations.
data Statement
  = Assume Name Type                 -- assume x : t
  | Define Name Type (Term Untyped)  -- define x : t = e
  | Import FilePath                  -- import m
  deriving stock (Show, Read)

-- | Terms are λ-calculus expressions in Administrative Normal Form (ANF).
data Term a
  = Val Value                       PV a  -- x
  | App (Term a) Name               PV a -- e x
  | Lam Name Type (Term a)          PV a -- \x:t. e   -- TODO: unrefined type only?
  | Let Name (Term a) (Term a)      PV a -- let x = e1 in e2
  | Rec Name Type (Term a) (Term a) PV a -- rec x : t = e1 in e2
  | If Name (Term a) (Term a)       PV a -- if x then e1 else e2
  deriving stock (Show, Read)

type Untyped = ()
type Typed = (Type, Con)
-- TODO: type Verified = (Type, Con, Assignment)

data Value
  = U PV          -- unit
  | B Bool PV     -- true, false
  | I Integer PV  -- 0, -1, 1, ...
  | S Text PV     -- "lorem ipsum"
  | V Name        -- x
  deriving stock (Show, Read)

-- | Equality between values ignores provenance.
instance Eq Value where
  U   _ == U   _ = True
  B a _ == B b _ = a == b
  I a _ == I b _ = a == b
  S a _ == S b _ = a == b
  V a   == V b   = a == b
  _     == _     = False

instance HasProvenance Value where
  getPV (U pv) = pv
  getPV (B _ pv) = pv
  getPV (I _ pv) = pv
  getPV (S _ pv) = pv
  getPV (V x) = getPV x
  setPV pv (U _) = U pv
  setPV pv (B x _) = B x pv
  setPV pv (I x _) = I x pv
  setPV pv (S x _) = S x pv
  setPV pv (V x) = V (setPV pv x)


-- | A type is either a refined 'Base' type or a dependent function type.
-- 
-- For convenience, we define the following equivalences:
--
-- >            b  ≡  {_:b|true}
-- >      t₁ → t₂  ≡  _:t₁ → t₂
-- > {x:b|r} → t₂  ≡  x:{x:b|r} → t₂
--
data Type
  = TBase Name Base Reft PV  -- {v:b|r}
  | TFun Name Type Type PV   -- x:t₁ → t₂
  deriving stock (Show, Read)


isBaseType :: Type -> Bool
isBaseType (TBase _ _ _ _) = True
isBaseType _ = False

-- | A primitive base type.
data Base
  = TUnit
  | TBool
  | TInt
  | TString
  deriving stock (Ord, Eq, Show, Read)

data Reft
  = Unknown     -- ?
  | Known Pred  -- p
  deriving stock (Eq, Show, Read)

instance HasProvenance Type where
  getPV (TBase _ _ _ pv) = pv
  getPV (TFun _ _ _ pv) = pv
  setPV pv (TBase v b r _) = TBase v b r pv
  setPV pv (TFun x t1 t2 _) = TFun x t1 t2 pv

------------------------------------------------------------------------------
-- Predicates

data Pred
  = PVal Value          -- x
  | PBin Bop Pred Pred  -- p1 o p2
  | PRel Rel Pred Pred  -- p1 R p2
  | PAnd [Pred]         -- p1 /\ p2 /\ ... /\ pn
  | PDisj Pred Pred     -- p1 \/ p2
  | PImpl Pred Pred     -- p1 ==> p2
  | PIff Pred Pred      -- p1 <=> p2
  | PNot Pred           -- ~p1
  | PFun Name [Pred]    -- f(p1,p2,...)
  | PAppK KVar [Name]   -- ^ κ-variable application @κᵢ(x₁,x₂,…)@  
  | PExists Name Base Pred  -- exists x:b. p
  deriving stock (Eq, Show, Read)

data Bop = Add | Sub | Mul | Div
  deriving stock (Eq, Show, Read)

data Rel = Eq | Neq | Geq | Leq | Gt | Lt
  deriving stock (Eq, Show, Read)

pattern PTrue :: PV -> Pred
pattern PTrue pv = PVal (B True pv)

pattern PFalse :: PV -> Pred
pattern PFalse pv = PVal (B False pv)

pVar :: Name -> Pred
pVar = PVal . V

pEq :: Pred -> Pred -> Pred
pEq = PRel Eq

-- | Smart constructor for `PAnd`, eliminates redundant values and merges
-- adjacent `PAnd` lists.
pAnd :: Pred -> Pred -> Pred
pAnd (PTrue _)   q           = q
pAnd (PFalse pv) _           = PFalse pv
pAnd p           (PTrue _)   = p
pAnd _           (PFalse pv) = PFalse pv
pAnd (PAnd ps)   (PAnd qs)   = PAnd (ps ++ qs)
pAnd (PAnd ps)   q           = PAnd (ps ++ [q])
pAnd p           (PAnd qs)   = PAnd (p:qs)
pAnd p           q           = PAnd [p,q]

-- | Smart constructor for `PDisj`, eliminates redundant values.
pOr :: Pred -> Pred -> Pred
pOr (PFalse _) q          = q
pOr (PTrue pv) _          = PTrue pv
pOr p          (PFalse _) = p
pOr _          (PTrue pv) = PTrue pv
pOr p          q          = PDisj p q

------------------------------------------------------------------------------
-- Constraints

-- | Constraints are Horn clauses in negation normal form (NNF). They form a
-- tree, where each leaf is a goal ('CHead') and each node either quantifies
-- some variable to satisfy a hypothesis ('CAll') or conjoins two
-- sub-constraints ('CAnd').
data Con
  = CHead Pred               -- p
  | CAnd Con Con             -- c1 /\ c2
  | CAll Name Base Pred Con  -- forall x:b. p ==> c
  deriving stock (Eq, Show, Read)

pattern CTrue :: PV -> Con
pattern CTrue pv = CHead (PTrue pv)

-- | Smart constructor for `CAnd`, eliminates redundant true values.
cAnd :: Con -> Con -> Con
cAnd (CTrue _) c2        = c2
cAnd c1        (CTrue _) = c1
cAnd c1        c2        = CAnd c1 c2

------------------------------------------------------------------------------
-- K vars

-- | A /refinement variable/ κ represents an unknown refinment over some free
-- variables z₁,z₂,…,zₙ, known as the /parameters/ of κ. For any κ-variable, we
-- know its arity (the number of parameters) as well as the expected parameter
-- types τ₁,τ₂,…,τₙ.
-- 
-- Refinement variables can occur in predicates in the form of applications
-- ('PAppK') which bind the parameters to particular values. Note that the
-- refinement represented by an applied κ-variable is unknown until the solving
-- phase, where we aim to find some assignment σ that maps each κ-variable to a
-- concrete refinement predicate satisfying the constraints induced by the
-- applications.
--
-- In the literature, κ-variables are sometimes referred to as /Horn variables/.
data KVar = KVar Int [Base]
  deriving stock (Ord, Eq, Show, Read)

-- | The parameters of a κ-variable.
kparams :: KVar -> [Name]
kparams (KVar _ ts) = [fromString $ "z" ++ show @Int i | i <- [0..length ts]]

class HasKVars a where
  -- | The set of κ-variables in a constraint or predicate.
  kvars :: a -> Set KVar

instance HasKVars Pred where
  kvars = \case
    PAppK k _ -> Set.singleton k
    PBin _ p1 p2 -> kvars p1 <> kvars p2
    PRel _ p1 p2 -> kvars p1 <> kvars p2
    PAnd ps      -> foldMap kvars ps
    PDisj p1 p2  -> kvars p1 <> kvars p2
    PImpl p1 p2  -> kvars p1 <> kvars p2
    PIff p1 p2   -> kvars p1 <> kvars p2
    PNot p       -> kvars p
    _            -> mempty

instance HasKVars Con where
  kvars = \case
    CHead p      -> kvars p
    CAnd c1 c2   -> kvars c1 <> kvars c2
    CAll _ _ p c -> kvars p <> kvars c

instance (Foldable t, HasKVars a) => HasKVars (t a) where
  kvars = foldMap kvars
