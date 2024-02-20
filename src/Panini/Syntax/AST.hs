-- TODO: module documentation
{-# LANGUAGE OverloadedLists #-}
module Panini.Syntax.AST where

import Data.Generics.Uniplate.Direct
import Data.Set ((\\))
import Panini.Pretty
import Panini.Provenance
import Panini.Syntax.Expressions
import Panini.Syntax.Names
import Panini.Syntax.Predicates
import Panini.Syntax.Primitives
import Panini.Syntax.Substitution
import Prelude

------------------------------------------------------------------------------

-- | A program is simply a list of successive statements.
type Program = [Statement]

-- | Statements are top-level declarations.
data Statement
  = Assume Name Type    -- x : t
  | Define Name Term    -- x = e
  | Import FilePath PV  -- import m
  deriving stock (Show, Read)

instance Pretty Program where
  pretty = vcat . map pretty

instance Pretty Statement where
  pretty = \case
    Assume x t -> pretty x <+> ":" <+> pretty t
    Define x e -> pretty x <+> "=" <+> pretty e
    Import m _ -> ann Keyword "import" <+> pretty m

------------------------------------------------------------------------------

-- | Terms are λ-calculus expressions in A-Normal Form (ANF).
data Term
  = Val Atom                    -- ^ value @v@
  | App Term Atom           PV  -- ^ application @e v@
  | Lam Name Type Term      PV  -- ^ abstraction @λx:t. e@
  | Let Name Term Term      PV  -- ^ binding @let x = e1 in e2@
  | Rec Name Type Term Term PV  -- ^ recursion @rec x : t = e1 in e2@
  | If Atom Term Term       PV  -- ^ branch @if v then e1 else e2@
  deriving stock (Show, Read)

-- | Atomic values are either constants @c@ or variables @x@.
data Atom
  = Con Value  -- ^ constant value
  | Var Name   -- ^ variable
  deriving stock (Show, Read)

instance Pretty Term where
  pretty = \case
    Val v -> pretty v
    
    App e x _ -> pretty e <+> pretty x  
    
    Lam x t e _ -> 
      nest 2 $ group $ 
      lambda <> pretty x <> ":" <> parensIf (isFun t) (pretty t) <> dot <\>
        pretty e

    Let x e1 e2 _ -> 
      ann Keyword "let" <+> pretty x <+> symEq <+> group (pretty e1 <\> 
      ann Keyword "in") <\\> 
      pretty e2
  
    Rec x t e1 e2 _ ->
      ann Keyword "rec" <+> pretty x <+> ":" <+> pretty t <\> 
      symEq <+> group (pretty e1 <\> 
      ann Keyword "in") <\\> 
      pretty e2
  
    If x e1 e2 _ -> group $
      ann Keyword "if" <+> pretty x <+> nest 2 (ann Keyword "then" <\> 
        pretty e1) <\> 
      nest 2 (ann Keyword "else" <\> 
        pretty e2)

instance Pretty Atom where
  pretty = \case
    Con v -> pretty v
    Var x -> pretty x

instance HasProvenance Term where
  getPV = \case
    Val v          -> getPV v
    App _ _     pv -> pv
    Lam _ _ _   pv -> pv
    Let _ _ _   pv -> pv
    Rec _ _ _ _ pv -> pv
    If _ _ _    pv -> pv
  setPV pv = \case
    Val v           -> Val (setPV pv v)
    App e v       _ -> App e v pv
    Lam x t e     _ -> Lam x t e pv
    Let x e1 e2   _ -> Let x e1 e2 pv
    Rec x t e1 e2 _ -> Rec x t e1 e2 pv
    If v e1 e2    _ -> If v e1 e2 pv

instance HasProvenance Atom where
  getPV = \case
    Con c -> getPV c
    Var x -> getPV x
  setPV pv = \case
    Con c -> Con $ setPV pv c
    Var x -> Var $ setPV pv x

instance Subable Atom Atom where
  subst x y = \case
    Var n | y == n -> x
    v              -> v
  freeVars = \case
    Var n -> [n]
    Con _ -> []

------------------------------------------------------------------------------

-- | A type is either a refined 'Base' type or a dependent function type.
data Type
  = TBase Name Base Reft PV  -- {v:b|r}
  | TFun Name Type Type PV   -- x:t₁ → t₂
  deriving stock (Show, Read)

-- | Syntactic equality, ignoring provenance.
instance Eq Type where
  TBase x1 b1 r1 _ == TBase x2 b2 r2 _ = x1 == x2 && b1 == b2 && r1 == r2
  TFun x1 s1 t1 _  == TFun x2 s2 t2 _  = x1 == x2 && s1 == s2 && t1 == t2
  _                == _                = False

-- | When pretty printing a type, we try to be as succinct as possible. 
--
-- We use the following notational equivalences, as long as this will not result
-- in incorrect type signatures or loss of user-supplied information:
--
-- >            b  ≡  {_:b|true}
-- >      t₁ → t₂  ≡  _:t₁ → t₂
-- > {x:b|r} → t₂  ≡  x:{x:b|r} → t₂
--
instance Pretty Type where
  pretty = \case
    -- {_:b|true}
    TBase v b (Known PTrue) _ | isDummy v 
      -> pretty b

    -- {v:b|r}
    TBase v b r _ 
      -> ppBaseReft v b r

    -- _:{_:b|true} → t₂
    TFun x (TBase v b (Known PTrue) _) t2 _ 
      | isDummy x, isDummy v, x `notElem` freeVars t2
      -> pretty b `arr` pretty t2

    -- x:{x:b|r} → t₂
    TFun x (TBase v b r _) t2 _ | x == v 
      -> ppBaseReft v b r `arr` pretty t2
    
    -- x:{v:b|r} → t₂
    TFun x t1@(TBase _ _ _ _) t2 _ 
      -> x `col` pretty t1 `arr` pretty t2
      
    -- _:(s₁ → s₂) → t₁  ≡  (s₁ → s₂) → t₁
    TFun x t1@(TFun _ _ _ _) t2 _ | isDummy x, x `notElem` freeVars t2
      -> parens (pretty t1) `arr` pretty t2
    
    -- x:(s₁ → s₂) → t₁  ≡  x:(s₁ → s₂) → t₁
    TFun x t1@(TFun _ _ _ _) t2 _ 
      -> x `col` parens (pretty t1) `arr` pretty t2
   
   where
    ppBaseReft v b r = braces $ v `col` pretty b <+> mid <+> pretty r    
    arr a b = a <+> arrow <+> b
    col x a = pretty x <> colon <> a

instance HasProvenance Type where
  getPV (TBase _ _ _ pv) = pv
  getPV (TFun _ _ _ pv) = pv
  setPV pv (TBase v b r _) = TBase v b r pv
  setPV pv (TFun x t1 t2 _) = TFun x t1 t2 pv

-- see Panini.Syntax.Substitution
instance Subable Type Expr where
  subst x y = \case
    -- In a refined base type {n:b|r}, the value variable n names the
    -- value of type b that is being refined. Thus, we take n to be bound in r.    
    TBase n b r pv
      | y == n      -> TBase n b            r  pv  -- (1)
      | x == EVar n -> TBase ṅ b (subst x y ṙ) pv  -- (2)
      | otherwise   -> TBase n b (subst x y r) pv  -- (3)
      where
        ṙ = subst (EVar ṅ) n r
        ṅ = freshName n ([y] <> freeVars r)

    -- In a dependent function type (n:t₁) → t₂, the name n binds t₁ in t₂. 
    -- Note that t₁ might itself contain (free) occurrences of n.
    TFun n t₁ t₂ pv
      | y == n      -> TFun n (subst x y t₁)            t₂  pv  -- (1)
      | x == EVar n -> TFun ṅ (subst x y t₁) (subst x y t₂̇) pv  -- (2)
      | otherwise   -> TFun n (subst x y t₁) (subst x y t₂) pv  -- (3)
      where
        t₂̇ = subst (EVar ṅ) n t₂
        ṅ = freshName n ([y] <> freeVars t₂)

  freeVars = \case
    TBase v _ r _ -> freeVars r \\ [v]
    TFun x t₁ t₂ _ -> (freeVars t₁ <> freeVars t₂) \\ [x]

instance Biplate Type Pred where
  biplate = \case
    TBase x b r pv -> plate TBase |- x |- b |+ r |- pv
    TFun x t1 t2 pv -> plate TFun |- x |+ t1 |+ t2 |- pv 

instance Biplate Type Reft where
  biplate = \case
    TBase x b r pv  -> plate TBase |- x |- b |* r |- pv
    TFun x t1 t2 pv -> plate TFun |- x |+ t1 |+ t2 |- pv

isFun :: Type -> Bool
isFun (TFun _ _ _ _) = True
isFun _              = False

------------------------------------------------------------------------------

data Reft
  = Unknown     -- ?
  | Known Pred  -- p
  deriving stock (Eq, Show, Read)

instance Pretty Reft where
  pretty Unknown   = "?"
  pretty (Known p) = pretty p

instance Subable Reft Expr where
  subst x y = descendBi (subst @Pred x y)  
  freeVars = mconcat . map (freeVars @Pred) . universeBi

instance Uniplate Reft where
  uniplate = \case
    Unknown -> plate Unknown
    Known p -> plate Known |- p

instance Biplate Reft Pred where
  biplate = \case
    Unknown -> plate Unknown
    Known p -> plate Known |* p
