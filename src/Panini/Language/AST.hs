-- TODO: module documentation
module Panini.Language.AST where

import Panini.Logic.Predicates
import Panini.Names
import Panini.Pretty.Printer
import Panini.Primitives
import Panini.Provenance
import Prelude

------------------------------------------------------------------------------

-- | A program is simply a list of successive statements.
type Program = [Statement]

-- | Statements are top-level declarations.
data Statement
  = Assume Name Type       -- x : t
  | Define Name Type Term  -- x : t = e
  | Import FilePath PV     -- import m
  deriving stock (Show, Read)

instance Pretty Program where
  pretty = vcat . map pretty

instance Pretty Statement where
  pretty = \case
    Assume x t   -> pretty x <+> symColon <+> pretty t
    Define x t e -> pretty x <+> symColon <+> pretty t 
                    <\> symEq <+> pretty e
    Import m _   -> keyword "import" <+> pretty m

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
  pretty (Val v) = pretty v
  
  pretty (App e x _) = pretty e <+> pretty x  
  
  pretty (Lam x t e _) = 
    nest 2 $ group $ symLambda <> pretty x <> symColon <> pretty t <> symDot 
                     <\> pretty e
  
  pretty (Let x e1 e2 _) = 
    keyword "let" <+> pretty x <+> symEq <+> group (pretty e1 <\> keyword "in") 
    <\\> pretty e2
  
  pretty (Rec x t e1 e2 _) =
    keyword "rec" <+> pretty x <+> symColon <+> pretty t 
    <\> symEq <+> group (pretty e1 <\> keyword "in") 
    <\\> pretty e2
  
  pretty (If x e1 e2 _) = group $
    keyword "if" <+> pretty x <+> nest 2 (keyword "then" <\> pretty e1) 
                              <\> nest 2 (keyword "else" <\> pretty e2)

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
      | x == v, isT r, isDummy x ->         pretty t  `arr` pretty t2
      | x == v, isT r            -> x `col` pretty t  `arr` pretty t2
      | x == v                   ->         pretty t1 `arr` pretty t2
  
    TFun x t1@(TFun _ _ _ _) t2 _
      | isDummy x ->         parens (pretty t1) `arr` pretty t2
      | otherwise -> x `col` parens (pretty t1) `arr` pretty t2
  
    TFun x t1 t2 _
      | isDummy x ->         pretty t1 `arr` pretty t2
      | otherwise -> x `col` pretty t1 `arr` pretty t2

    TBase v t r _
      | isT r, isDummy v ->                  pretty t
      | otherwise        -> braces $ v `col` pretty t <+> "|" <+> pretty r
   
   where    
    isT (Known PTrue) = True
    isT _             = False
    
    arr a b = a <+> symArrow <+> b
    col x a = pretty x <> symColon <> a

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
