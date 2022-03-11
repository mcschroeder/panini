{-# LANGUAGE OverloadedStrings #-}

module Panini.Core.Printer 
  ( PrintOptions(..)
  , printProg
  , printDecl
  , printExpr
  , printType
  , printCon
  , printTypeError
  ) where

import Control.Monad
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as Text
import Panini.Core.Checker
import Panini.Core.Syntax
import Prelude
import Prettyprinter
import Prettyprinter.Render.Util.SimpleDocTree
import System.Console.ANSI

-------------------------------------------------------------------------------

data PrintOptions = PrintOptions 
  { ansiColors     :: Bool
  , unicodeSymbols :: Bool
  , fixedWidth     :: Maybe Int
  }

printProg :: PrintOptions -> Prog -> Text
printProg opts = prettyPrint opts . pProg

printDecl :: PrintOptions -> Decl -> Text
printDecl opts = prettyPrint opts . pDecl

printExpr :: PrintOptions -> Expr -> Text
printExpr opts = prettyPrint opts . pExpr

printType :: PrintOptions -> Type -> Text
printType opts = prettyPrint opts . pType

printCon :: PrintOptions -> Con -> Text
printCon opts = prettyPrint opts . annotate Predicate . pCon

printTypeError :: PrintOptions -> FilePath -> TypeError -> Text
printTypeError opts fp err = 
  let loc = pretty fp <> ":"
      typ = annotate Error "error:"
      str = annotate Message (loc <+> typ) <\\> pTypeError err
  in prettyPrint opts $ nest 2 str

-------------------------------------------------------------------------------

prettyPrint :: PrintOptions -> Doc Ann -> Text
prettyPrint o = 
  renderSimplyDecorated renderT renderA . treeForm . layoutSmart layoutOpt
 where
  layoutOpt = defaultLayoutOptions { layoutPageWidth = pw }
  pw = maybe Unbounded (\w -> AvailablePerLine w 1) o.fixedWidth
  renderT = id
  renderA = liftM2 (.) 
      (if o.ansiColors     then colorize  else const id) 
      (if o.unicodeSymbols then unicodify else const id)

colorize :: Ann -> Text -> Text
colorize = \case
  Keyword   -> sgr [SetConsoleIntensity BoldIntensity]
  Predicate -> sgr [SetColor Foreground Vivid Blue]
  Message   -> sgr [SetConsoleIntensity BoldIntensity]
  Error     -> sgr [SetColor Foreground Vivid Red]
  _         -> id
 where
  sgr c t = Text.pack (setSGRCode c) <> t <> Text.pack (setSGRCode [Reset])

unicodify :: Ann -> Text -> Text
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
unicodify _ = id

-------------------------------------------------------------------------------

data Ann = Keyword | Symbol | Predicate | Message | Error

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

pName :: Name -> Doc ann
pName (Name x) = pretty x

-------------------------------------------------------------------------------

pProg :: Prog -> Doc Ann
pProg = vcat . map pDecl

pDecl :: Decl -> Doc Ann
pDecl = \case
  Assume x t -> kw "assume" <+> pName x <+> kws ":" <+> pType t
  Define x e -> kw "define" <+> pName x <+> kws "=" <+> pExpr e

-------------------------------------------------------------------------------

pExpr :: Expr -> Doc Ann
pExpr = \case
  Val x -> pValue x
  App e x -> pExpr e <+> pValue x    
  
  Lam x e -> nest 2 $ group $ kws "\\" <> pName x <> kws "." <\> pExpr e
  
  Ann e t -> pExpr e <+> kws ":" <+> pType t

  Let x e1 e2 -> 
    kw "let" <+> pName x <+> kws "=" <+> group (pExpr e1 <\> kw "in") <\\>
    pExpr e2
  
  Rec x t e1 e2 -> 
    kw "rec" <+> pName x <+> kws ":" <+> pType t <\> 
    kws "=" <+> group (pExpr e1 <\> kw "in") <\\>
    pExpr e2
  
  If x e1 e2 -> group $
    kw "if" <+> pValue x <+> 
    nest 2 (kw "then" <\> pExpr e1) <\> 
    nest 2 (kw "else" <\> pExpr e2)

  Ass x t e ->
    kw "assume" <+> pName x <+> kws ":" <+> pType t <+> kw "in" <\\>
    pExpr e

pValue :: Value -> Doc Ann
pValue = \case
  U -> "unit"
  B True -> "true"
  B False -> "false"
  I c -> pretty c
  S t -> viaShow t
  V x -> pName x

-------------------------------------------------------------------------------

isT :: Reft -> Bool
isT (Known (PVal (B True))) = True
isT _ = False

arr :: Doc Ann -> Doc Ann -> Doc Ann
arr a b = a <+> sym "->" <+> b

col :: Name -> Doc Ann -> Doc Ann
col x a = pName x <> sym ":" <> a

pType :: Type -> Doc Ann
pType = \case
  TFun x t1@(TBase v t r) t2 
    | x == v, isT r, isDummy x ->         pBaseTy t `arr` pType t2
    | x == v, isT r            -> x `col` pBaseTy t `arr` pType t2
    | x == v                   ->         pType t1  `arr` pType t2
  
  TFun x t1@(TFun _ _ _) t2 
    | isDummy x ->         parens (pType t1) `arr` pType t2
    | otherwise -> x `col` parens (pType t1) `arr` pType t2
  
  TFun x t1 t2     
    | isDummy x ->         pType t1 `arr` pType t2
    | otherwise -> x `col` pType t1 `arr` pType t2

  TBase v t r 
    | isT r, isDummy v ->                  pBaseTy t
    | otherwise        -> braces $ v `col` pBaseTy t <+> "|" <+> pReft r

pBaseTy :: Base -> Doc Ann
pBaseTy = \case
  TUnit -> "unit"
  TBool -> "bool"
  TInt -> "int"
  TString -> "string"

-------------------------------------------------------------------------------

pReft :: Reft -> Doc Ann
pReft = \case
  Unknown -> sym "?"
  Known p -> annotate Predicate $ pPred p

pPred :: Pred -> Doc Ann
pPred p0 = case p0 of
  PVal x -> pValue x
  PFun f ps -> pName f <> (parens $ mconcat $ intersperse "," $ map pPred ps)
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
prettyUnary o l docO = docO <> parensIf (pO > pL) (pPred l)
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
   docL = pPred l
   docR = pPred r

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

pCon :: Con -> Doc Ann
pCon = \case
  CPred p -> parensIf (hasLogic p) (pPred p)
  CConj c1 c2 -> pCon c1  <+> sym "/\\" <+> pCon c2 
  CAll x b p c  -> 
    sym "forall " <> pName x <> sym ":" <> pBaseTy b <> sym "." <+> pPred p <+>
    sym "==>" <+>
    pCon c    

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

pTypeError :: TypeError -> Doc Ann
pTypeError = bullets . \case

  InvalidSubtypingBase (t1,b1) (t2,b2) ->
    [ pBaseTy b1 <+> msg "is not a subtype of" <+> pBaseTy b2
    , group $ nest 4 (msg "Therefore," <\> pType t1) <\> 
              nest 4 (msg "is not a subtype of" <\> pType t2)
    ]

  InvalidSubtyping t1 t2 ->
    [ pType t1 <+> msg "is not a subtype of" <+> pType t2]

  VarNotInScope n -> [msg "Variable not in scope:" <+> pName n]
  
  ExpectedFunType e t -> 
    [ pType t <+> msg "is not a function type"
    , group $ nest 4 $ msg "Expected a function type for expression:" <\> pExpr e
    ]

  NoSynth e ->
    [ nest 4 $ group $ msg "Can't synthesize type for expression:" <\> 
      pExpr e
    ]

msg :: Text -> Doc Ann
msg = annotate Message . pretty

bullets :: [Doc ann] -> Doc ann
bullets = mconcat . intersperse "\n" . map ("•" <+>)
