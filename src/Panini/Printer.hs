{-# LANGUAGE OverloadedStrings #-}

module Panini.Printer 
  ( Pretty(..)
  , RenderOptions(..)
  , renderDoc
  ) where

import Control.Monad
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as Text
import Panini.Error
import Panini.Provenance
import Panini.Syntax
import Prelude
import Prettyprinter hiding (Pretty(..))
import Prettyprinter qualified
import Prettyprinter.Render.Util.SimpleDocTree
import System.Console.ANSI

-------------------------------------------------------------------------------

data Ann = Keyword | Symbol | Predicate | Message | Error | Margin

-- | Our custom pretty-printing class, fixing `Doc` annotations to `Ann`.
class Pretty a where
  pretty :: a -> Doc Ann

instance Pretty Text where pretty = Prettyprinter.pretty
instance Pretty String where pretty = Prettyprinter.pretty
instance Pretty Integer where pretty = Prettyprinter.pretty
instance Pretty Int where pretty = Prettyprinter.pretty

instance (Pretty a, Pretty b) => Pretty (a, b) where 
  pretty (a,b) = "(" <> pretty a <> "," <+> pretty b <> ")"

-------------------------------------------------------------------------------

data RenderOptions = RenderOptions 
  { ansiColors     :: Bool
  , unicodeSymbols :: Bool
  , fixedWidth     :: Maybe Int
  }

renderDoc :: RenderOptions -> Doc Ann -> Text
renderDoc opts = 
  renderSimplyDecorated renderT renderA . treeForm . layoutSmart layoutOpt
 where
  layoutOpt = defaultLayoutOptions { layoutPageWidth = pw }
  pw = maybe Unbounded (\w -> AvailablePerLine w 1) opts.fixedWidth
  renderT = id
  renderA = liftM2 (.) 
      (if opts.ansiColors     then colorize  else const id) 
      (if opts.unicodeSymbols then unicodify else const id)

  colorize = \case
    Keyword   -> sgr [SetConsoleIntensity BoldIntensity]
    Predicate -> sgr [SetColor Foreground Vivid Blue]
    Message   -> sgr [SetConsoleIntensity BoldIntensity]
    Error     -> sgr [SetColor Foreground Vivid Red]
    Margin    -> sgr [SetColor Foreground Dull Blue]
    _         -> id
 
  sgr c t = Text.pack (setSGRCode c) <> t <> Text.pack (setSGRCode [Reset])

  unicodify Symbol = \case
    "/\\" -> "∧"
    "\\/" -> "∨"
    ">="  -> "≥"
    "<="  -> "≤"
    "~"   -> "¬"
    "/="  -> "≠"
    "==>" -> "⇒"
    "<=>" -> "⇔"
    "->"  -> "→"
    "\\"  -> "λ"
    "forall " -> "∀"  -- note the extra space
    x     -> x
  unicodify Margin = Text.replace "|" "│"
  unicodify _ = id

-------------------------------------------------------------------------------

kw :: Text -> Doc Ann
kw = annotate Keyword . pretty

sym :: Text -> Doc Ann
sym = annotate Symbol . pretty

kws :: Text -> Doc Ann
kws = annotate Keyword . annotate Symbol . pretty

-- | Inserts a linebreak.
(<\>) :: Doc ann -> Doc ann -> Doc ann
a <\> b = a <> line <> b

-- | Inserts a hard linebreak.
(<\\>) :: Doc ann -> Doc ann -> Doc ann
a <\\> b = a <> hardline <> b

-------------------------------------------------------------------------------

instance Pretty Name where
  pretty (Name _ x) = pretty x

-------------------------------------------------------------------------------

instance Pretty Prog where
  pretty = vcat . map pretty

instance Pretty Decl where
  pretty (Assume x t) = kw "assume" <+> pretty x <+> kws ":" <+> pretty t
  pretty (Define x e) = kw "define" <+> pretty x <+> kws "=" <+> pretty e

-------------------------------------------------------------------------------

instance Pretty Expr where
  pretty (Val x) = pretty x
  pretty (App e x) = pretty e <+> pretty x  
  pretty (Lam x e) = nest 2 $ group $ kws "\\" <> pretty x <> kws "." <\> pretty e
  pretty (Ann e t) = pretty e <+> kws ":" <+> pretty t
  pretty (Let x e1 e2) = 
    kw "let" <+> pretty x <+> kws "=" <+> group (pretty e1 <\> kw "in") <\\> 
    pretty e2
  
  pretty (Rec x t e1 e2) =
    kw "rec" <+> pretty x <+> kws ":" <+> pretty t <\> 
    kws "=" <+> group (pretty e1 <\> kw "in") <\\>
    pretty e2
  
  pretty (If x e1 e2) = group $
    kw "if" <+> pretty x <+> 
    nest 2 (kw "then" <\> pretty e1) <\> 
    nest 2 (kw "else" <\> pretty e2)

  pretty (Ass x t e) =
    kw "assume" <+> pretty x <+> kws ":" <+> pretty t <+> kw "in" <\\>
    pretty e

instance Pretty Value where
  pretty U = "unit"
  pretty (B True) = "true"
  pretty (B False) = "false"
  pretty (I c) = pretty c
  pretty (S t) = viaShow t
  pretty (V x) = pretty x

-------------------------------------------------------------------------------

isT :: Reft -> Bool
isT (Known (PVal (B True))) = True
isT _ = False

arr :: Doc Ann -> Doc Ann -> Doc Ann
arr a b = a <+> sym "->" <+> b

col :: Name -> Doc Ann -> Doc Ann
col x a = pretty x <> sym ":" <> a

instance Pretty Type where
  pretty (TFun x t1@(TBase v t r) t2)
    | x == v, isT r, isDummy x =         pretty t  `arr` pretty t2
    | x == v, isT r            = x `col` pretty t  `arr` pretty t2
    | x == v                   =         pretty t1 `arr` pretty t2
  
  pretty (TFun x t1@(TFun _ _ _) t2)
    | isDummy x =         parens (pretty t1) `arr` pretty t2
    | otherwise = x `col` parens (pretty t1) `arr` pretty t2
  
  pretty (TFun x t1 t2)
    | isDummy x =         pretty t1 `arr` pretty t2
    | otherwise = x `col` pretty t1 `arr` pretty t2

  pretty (TBase v t r)
    | isT r, isDummy v =                  pretty t
    | otherwise        = braces $ v `col` pretty t <+> "|" <+> pretty r

instance Pretty Base where
  pretty TUnit = "unit"
  pretty TBool = "bool"
  pretty TInt = "int"
  pretty TString = "string"

-------------------------------------------------------------------------------

instance Pretty Reft where
  pretty Unknown = sym "?"
  pretty (Known p) = pretty p

instance Pretty Pred where
  pretty p0 = annotate Predicate $ case p0 of
    PVal x -> pretty x
    PFun f ps -> pretty f <> (parens $ mconcat $ intersperse "," $ map pretty ps)
    PNot p1 -> prettyUnary p0 p1 (sym "~")
    PBin Mul p1 p2 -> prettyOp p0 p1 p2 (sym "*")
    PBin Div p1 p2 -> prettyOp p0 p1 p2 (sym "/")
    PBin Add p1 p2 -> prettyOp p0 p1 p2 (sym "+")
    PBin Sub p1 p2 -> prettyOp p0 p1 p2 (sym "-")
    PRel Neq p1 p2 -> prettyOp p0 p1 p2 (sym "/=")
    PRel Eq p1 p2  -> prettyOp p0 p1 p2 (sym "=")
    PRel Leq p1 p2 -> prettyOp p0 p1 p2 (sym "<=")
    PRel Lt p1 p2  -> prettyOp p0 p1 p2 (sym "<")
    PRel Geq p1 p2 -> prettyOp p0 p1 p2 (sym ">=")
    PRel Gt p1 p2  -> prettyOp p0 p1 p2 (sym ">")
    PConj p1 p2    -> prettyOp p0 p1 p2 (sym "/\\")
    PDisj p1 p2    -> prettyOp p0 p1 p2 (sym "\\/")
    PImpl p1 p2    -> prettyOp p0 p1 p2 (sym "==>")
    PIff p1 p2     -> prettyOp p0 p1 p2 (sym "<=>")

prettyUnary :: Pred -> Pred -> Doc Ann -> Doc Ann
prettyUnary o l docO = docO <> parensIf (pO > pL) (pretty l)
 where
   (pO, _) = fixity o
   (pL, _) = fixity l

prettyOp :: Pred -> Pred -> Pred -> Doc Ann -> Doc Ann
prettyOp o l r docO = case a of
  InfixL -> parensIf (pO > pL)  docL <+> docO <+> parensIf (pO >= pR) docR
  InfixN -> parensIf (pO >= pL) docL <+> docO <+> parensIf (pO >= pR) docR
  InfixR -> parensIf (pO >= pL) docL <+> docO <+> parensIf (pO > pR)  docR
  _ -> error "expected infix op"
 where
   (pO, a) = fixity o
   (pL, _) = fixity l
   (pR, _) = fixity r
   docL = pretty l
   docR = pretty r

parensIf :: Bool -> Doc ann -> Doc ann
parensIf x = if x then parens else id

class Fixity a where
  fixity :: a -> (Int, Assoc)

data Assoc = Prefix | InfixL | InfixN | InfixR
  deriving stock (Eq, Show, Read)

instance Fixity Pred where
  fixity (PNot _)       = (7, Prefix)
  fixity (PBin Mul _ _) = (6, InfixL)
  fixity (PBin Div _ _) = (6, InfixL)
  fixity (PBin Add _ _) = (5, InfixL)
  fixity (PBin Sub _ _) = (5, InfixL)
  fixity (PRel Eq _ _)  = (4, InfixN)
  fixity (PRel Neq _ _) = (4, InfixN)
  fixity (PRel Geq _ _) = (4, InfixN)
  fixity (PRel Leq _ _) = (4, InfixN)
  fixity (PRel Gt _ _)  = (4, InfixN)
  fixity (PRel Lt _ _)  = (4, InfixN)
  fixity (PConj _ _)    = (3, InfixR)
  fixity (PDisj _ _)    = (2, InfixR)
  fixity (PImpl _ _)    = (1, InfixN)
  fixity (PIff _ _)     = (1, InfixN)
  fixity _              = (9, InfixL)

-------------------------------------------------------------------------------

instance Pretty Con where
  pretty = annotate Predicate . \case
    CPred p -> parensIf (hasLogic p) (pretty p)
    CConj c1 c2 -> pretty c1  <+> sym "/\\" <+> pretty c2 
    CAll x b p c ->
      sym "forall " <> pretty x <> sym ":" <> pretty b <> sym "." <+> pretty p <+>
      sym "==>" <+>
      pretty c    

hasLogic :: Pred -> Bool
hasLogic = \case
  PConj _ _ -> True
  PDisj _ _ -> True
  PImpl _ _ -> True
  PIff _ _ -> True
  PBin _ p1 p2 -> hasLogic p1 && hasLogic p2
  PRel _ p1 p2 -> hasLogic p1 && hasLogic p2
  PNot p -> hasLogic p
  _ -> False

-------------------------------------------------------------------------------

instance Pretty Error where
  pretty e = case getPV e of
    FromSource loc (Just src) -> message (pretty loc) <\\> source loc src
    FromSource loc Nothing    -> message (pretty loc)
    NoPV                      -> message "<unknown location>"
    where
      message o  = nest 2 (header o <\\> reason)
      header o   = annotate Message (o <> ":" <+> annotate Error "error:")
      reason     = prettyErrorMessage e
      source l s = wavyDiagnostic l s

prettyErrorMessage :: Error -> Doc Ann
prettyErrorMessage = \case
  AlreadyDefined x -> pretty x <+> msg "is already defined"    
  VarNotInScope n -> msg "Variable not in scope:" <+> pretty n  
  MissingType n -> msg "Missing type definition for" <+> pretty n  
  
  InvalidSubtypeBase (t1,b1) (t2,b2) -> bullets
    [ pretty b1 <+> msg "is not a subtype of" <+> pretty b2
    , group $ nest 4 (msg "Therefore," <\> pretty t1) <\> 
      nest 4 (msg "is not a subtype of" <\> pretty t2)
    ]  
  
  InvalidSubtype t1 t2 -> 
    pretty t1 <+> msg "is not a subtype of" <+> pretty t2  
  
  ExpectedFunType e t -> bullets
    [ pretty t <+> msg "is not a function type"
    , group $ nest 4 $ msg "Expected a function type for expression:" <\> 
      pretty e
    ]

  CantSynth e -> 
    group $ nest 4 $ msg "Can't synthesize type for expression:" <\> pretty e

  ParserError _ e -> msg $ Text.stripEnd e

  where
    msg     = annotate Message . pretty @Text
    bullets = mconcat . intersperse "\n" . map ("•" <+>)

wavyDiagnostic :: SrcLoc -> Text -> Doc Ann
wavyDiagnostic (SrcLoc _ (l1,c1) (l2,c2)) s =
  annM (mPadding   <+> "|") <\\>
  annM (lineNumber <+> "|") <+> offendingLine <\\>
  annM (mPadding   <+> "|") <+> errorPointer
  where
    annM           = annotate Margin
    annE           = annotate Error
    mPadding       = pretty $ replicate (length (show l1)) ' '
    lineNumber     = pretty $ show l1
    offendingLine  = pretty lineL <> annE (pretty lineE) <> pretty lineR
    errorPointer   = pretty pPadding <> annE (pretty pointer)
    (lineL, s')    = Text.splitAt (c1 - 1) s
    (lineE, lineR) = Text.splitAt eLen s'
    pointer        = replicate pLen '^'
    pPadding       = if pLen > 0 then replicate pShift ' ' else ""
    pLen           = if pShift + eLen > sLen then sLen - pShift + 1 else eLen
    pShift         = c1 - 1
    eLen           = if l1 == l2 then c2 - c1 else 1
    sLen           = Text.length s
    
-------------------------------------------------------------------------------

instance Pretty SrcLoc where
  pretty (SrcLoc f (l1, c1) (l2, c2))
    | l1 == l2, c1 == c2 = flc
    | l1 == l2           = flc <> "-" <> pretty c2
    | otherwise          = flc <> "-" <> pretty l2 <> ":" <> pretty c2
    where
      flc = pretty f <> ":" <> pretty l1 <> ":" <> pretty c1
