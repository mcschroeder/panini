-- TODO: module documentation
module Panini.Syntax.AST where

import Panini.Pretty.Printer
import Panini.Syntax.Constraints
import Panini.Syntax.Names
import Panini.Syntax.Predicates
import Panini.Syntax.Primitives
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

instance Pretty Program where
  pretty = vcat . map pretty

instance Pretty Statement where
  pretty (Assume x t) = prettyKeyword "assume" <+> pretty x <+> prettyKeywordSymbol ":" <+> pretty t
  pretty (Define x t e) = 
    prettyKeyword "define" <+> pretty x <+> prettyKeyword ":" <+> pretty t <\> prettyKeywordSymbol "=" <+> pretty e
  pretty (Import m) = 
    prettyKeyword "import" <+> pretty m

------------------------------------------------------------------------------

-- | Terms are λ-calculus expressions in Administrative Normal Form (ANF).
data Term a
  = Con Constant                       a -- c
  | Var Name                           a -- x
  | App (Term a) Name               PV a -- e x
  | Lam Name Type (Term a)          PV a -- \x:t. e   -- TODO: unrefined type only?
  | Let Name (Term a) (Term a)      PV a -- let x = e1 in e2
  | Rec Name Type (Term a) (Term a) PV a -- rec x : t = e1 in e2
  | If Name (Term a) (Term a)       PV a -- if x then e1 else e2
  deriving stock (Show, Read)

type Untyped = ()
type Typed = (Type, Con)
-- TODO: type Verified = (Type, Con, Assignment)

instance Pretty (Term a) where
  pretty (Var x _) = pretty x
  pretty (Con c _) = pretty c
  pretty (App e x _ _) = pretty e <+> pretty x  
  pretty (Lam x t e _ _) = nest 2 $ group $ prettyKeywordSymbol "\\" <> pretty x <> prettyKeywordSymbol ":" <> pretty t <> prettyKeywordSymbol "." <\> pretty e
  pretty (Let x e1 e2 _ _) = 
    prettyKeyword "let" <+> pretty x <+> prettyKeywordSymbol "=" <+> group (pretty e1 <\> prettyKeyword "in") <\\> 
    pretty e2
  
  pretty (Rec x t e1 e2 _ _) =
    prettyKeyword "rec" <+> pretty x <+> prettyKeywordSymbol ":" <+> pretty t <\> 
    prettyKeywordSymbol "=" <+> group (pretty e1 <\> prettyKeyword "in") <\\>
    pretty e2
  
  pretty (If x e1 e2 _ _) = group $
    prettyKeyword "if" <+> pretty x <+> 
    nest 2 (prettyKeyword "then" <\> pretty e1) <\> 
    nest 2 (prettyKeyword "else" <\> pretty e2)

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

isBaseType :: Type -> Bool
isBaseType (TBase _ _ _ _) = True
isBaseType _ = False


instance Pretty Type where
  pretty (TFun x t1@(TBase v t r _) t2 _)
    | x == v, isT r, isDummy x =         pretty t  `arr` pretty t2
    | x == v, isT r            = x `col` pretty t  `arr` pretty t2
    | x == v                   =         pretty t1 `arr` pretty t2
  
  pretty (TFun x t1@(TFun _ _ _ _) t2 _)
    | isDummy x =         parens (pretty t1) `arr` pretty t2
    | otherwise = x `col` parens (pretty t1) `arr` pretty t2
  
  pretty (TFun x t1 t2 _)
    | isDummy x =         pretty t1 `arr` pretty t2
    | otherwise = x `col` pretty t1 `arr` pretty t2

  pretty (TBase v t r _)
    | isT r, isDummy v =                  pretty t
    | otherwise        = braces $ v `col` pretty t <+> "|" <+> pretty r


isT :: Reft -> Bool
isT (Known PTrue) = True
isT _ = False


arr :: Doc -> Doc -> Doc
arr a b = a <+> symArrow <+> b

col :: Name -> Doc -> Doc
col x a = pretty x <> symColon <> a


data Reft
  = Unknown     -- ?
  | Known Pred  -- p
  deriving stock (Eq, Show, Read)

instance Pretty Reft where
  pretty Unknown = prettySymbol "?"
  pretty (Known p) = pretty p

instance HasProvenance Type where
  getPV (TBase _ _ _ pv) = pv
  getPV (TFun _ _ _ pv) = pv
  setPV pv (TBase v b r _) = TBase v b r pv
  setPV pv (TFun x t1 t2 _) = TFun x t1 t2 pv
