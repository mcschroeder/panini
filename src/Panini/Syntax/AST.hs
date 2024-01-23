-- TODO: module documentation
module Panini.Syntax.AST where

import Data.Generics.Uniplate.Direct
import Panini.Pretty
import Panini.Provenance
import Panini.Syntax.Names
import Panini.Syntax.Predicates
import Panini.Syntax.Primitives
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

-- | Terms are λ-calculus expressions in Administrative Normal Form (ANF).
data Term
  = Val Value                  -- v
  | App Term Value          PV -- e v
  | Lam Name Type Term      PV -- \x:t. e   -- TODO: unrefined type only?
  | Let Name Term Term      PV -- let x = e1 in e2
  | Rec Name Type Term Term PV -- rec x : t = e1 in e2
  | If Value Term Term      PV -- if v then e1 else e2
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

------------------------------------------------------------------------------

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

instance Pretty Type where
  pretty = \case
    TFun x t1@(TBase v t r _) t2 _
      | x == v, isT r, isDummy x -> pretty t  `arr` pretty t2
      | x == v                   -> pretty t1 `arr` pretty t2
  
    TFun x t1@(TFun _ _ _ _) t2 _
      | isDummy x ->         parens (pretty t1) `arr` pretty t2
      | otherwise -> x `col` parens (pretty t1) `arr` pretty t2
  
    TFun x t1 t2 _
      | isDummy x ->         pretty t1 `arr` pretty t2
      | otherwise -> x `col` pretty t1 `arr` pretty t2

    TBase v t r _
      | isT r, isDummy v ->                  pretty t
      | otherwise        -> braces $ v `col` pretty t <+> mid <+> pretty r
   
   where    
    isT (Known PTrue) = True
    isT _             = False
    
    arr a b = a <+> arrow <+> b
    col x a = pretty x <> colon <> a

data Reft
  = Unknown     -- ?
  | Known Pred  -- p
  deriving stock (Eq, Show, Read)

instance Pretty Reft where
  pretty Unknown   = "?"
  pretty (Known p) = pretty p

instance HasProvenance Type where
  getPV (TBase _ _ _ pv) = pv
  getPV (TFun _ _ _ pv) = pv
  setPV pv (TBase v b r _) = TBase v b r pv
  setPV pv (TFun x t1 t2 _) = TFun x t1 t2 pv

-- | Syntactic equality, ignoring provenance.
instance Eq Type where
  TBase x1 b1 r1 _ == TBase x2 b2 r2 _ = x1 == x2 && b1 == b2 && r1 == r2
  TFun x1 s1 t1 _  == TFun x2 s2 t2 _  = x1 == x2 && s1 == s2 && t1 == t2
  _                == _                = False

instance Biplate Type Pred where
  biplate = \case
    TBase x b r pv -> plate TBase |- x |- b |+ r |- pv
    TFun x t1 t2 pv -> plate TFun |- x |+ t1 |+ t2 |- pv 

instance Biplate Type Reft where
  biplate = \case
    TBase x b r pv  -> plate TBase |- x |- b |* r |- pv
    TFun x t1 t2 pv -> plate TFun |- x |+ t1 |+ t2 |- pv

instance Uniplate Reft where
  uniplate = \case
    Unknown -> plate Unknown
    Known p -> plate Known |- p

instance Biplate Reft Pred where
  biplate = \case
    Unknown -> plate Unknown
    Known p -> plate Known |* p

isFun :: Type -> Bool
isFun (TFun _ _ _ _) = True
isFun _              = False
