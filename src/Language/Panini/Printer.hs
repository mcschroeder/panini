{-# LANGUAGE OverloadedStrings #-}

module Language.Panini.Printer (prettyPrint, prettyPut) where

import Data.Text (Text)
import Language.Panini.Syntax
import Prettyprinter
import Prettyprinter.Render.Util.SimpleDocTree
import System.Console.ANSI
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

-------------------------------------------------------------------------------

data Ann = Keyword | Symbol | Predicate

kw :: Text -> Doc Ann
kw = annotate Keyword . pretty

sym :: Text -> Doc Ann
sym = annotate Symbol . pretty

kws :: Text -> Doc Ann
kws = annotate Keyword . annotate Symbol . pretty

render :: SimpleDocStream Ann -> Text
render = renderSimplyDecorated id go . treeForm
 where
   go Keyword = sgr [SetConsoleIntensity BoldIntensity]
   go Symbol = unicodify
   go Predicate = sgr [SetColor Foreground Vivid Blue]

sgr :: [SGR] -> Text -> Text
sgr c t = Text.pack (setSGRCode c) <> t <> Text.pack (setSGRCode [Reset])

unicodify :: Text -> Text
unicodify = \case 
  "/\\" -> "∧"
  "\\/" -> "∨"
  ">="  -> "≥"
  "<="  -> "≤"
  "~"   -> "¬"
  "/="  -> "≠"
  "==>" -> "⇒"
  "<=>" -> "⇔"
  "->"  -> "→" 
  "\\" -> "λ"
  x     -> x

prettyPrint :: Int -> Expr -> Text
prettyPrint w = render . layoutSmart opts . pExpr
 where
   opts = defaultLayoutOptions { layoutPageWidth = AvailablePerLine w 1.0 }

prettyPut :: Expr -> IO ()
prettyPut e = do
  w <- maybe 80 snd <$> getTerminalSize
  Text.putStrLn $ prettyPrint w e

-- | Inserts a hard linebreak.
(<\>) :: Doc ann -> Doc ann -> Doc ann
a <\> b = a <> hardline <> b

-------------------------------------------------------------------------------

pName :: Name -> Doc ann
pName (Name x) = pretty x

-------------------------------------------------------------------------------

pExpr :: Expr -> Doc Ann
pExpr = \case
  Val x -> pValue x
  App e x -> pExpr e <+> pValue x    
  
  Lam x e -> nest 2 $ 
    kws "\\" <> pName x <> kws "." <\> 
    pExpr e
  
  Ann e t -> pExpr e <+> kws ":" <+> pType t
  
  Let x e1 e2 -> 
    kw "let" <+> pName x <+> kws "=" <+> pExpr e1 <> kw "in" <\> 
    pExpr e2
  
  Rec x t e1 e2 -> 
    kw "rec" <+> pName x <+> kws ":" <+> pType t <\> 
    kws "=" <+> pExpr e1 <\> 
    kw "in" <\> 
    pExpr e2
  
  If x e1 e2 -> 
    kw "if" <+> pValue x <+> 
    nest 2 (kw "then" <\> pExpr e1) <\> 
    nest 2 (kw "else" <\> pExpr e2)

pValue :: Value -> Doc Ann
pValue = \case
  U -> "unit"
  B True -> "true"
  B False -> "false"
  I c -> pretty c
  S t -> viaShow t
  V x -> pName x

-------------------------------------------------------------------------------

isPi :: Type -> Bool
isPi (Pi _ _ _) = True
isPi _ = False

isT :: Refinement -> Bool
isT (Known PTrue) = True
isT _ = False

arr :: Doc Ann -> Doc Ann -> Doc Ann
arr a b = a <+> sym "->" <+> b

col :: Name -> Doc Ann -> Doc Ann
col x a = pName x <> sym ":" <> a

pType :: Type -> Doc Ann
pType = \case
  Pi x t1@(Base y t r) t2 
    | x == y, isT r, isDummy x ->         pBaseTy t `arr` pType t2
    | x == y, isT r            -> x `col` pBaseTy t `arr` pType t2
    | x == y                   ->         pType t1  `arr` pType t2
  
  Pi x t1@(Pi _ _ _) t2 
    | isDummy x ->         parens (pType t1) `arr` pType t2
    | otherwise -> x `col` parens (pType t1) `arr` pType t2
  
  Pi x t1 t2     
    | isDummy x ->         pType t1 `arr` pType t2
    | otherwise -> x `col` pType t1 `arr` pType t2

  Base x t r 
    | isT r, isDummy x ->                  pBaseTy t
    | otherwise        -> braces $ x `col` pBaseTy t <+> "|" <+> pReft r

pBaseTy :: BaseType -> Doc Ann
pBaseTy = \case
  TyUnit -> "unit"
  TyBool -> "bool"
  TyInt -> "int"
  TyString -> "string"

-------------------------------------------------------------------------------

pReft :: Refinement -> Doc Ann
pReft = \case
  Unknown -> sym "?"
  Known p -> annotate Predicate $ pPred p

pPred :: Pred -> Doc Ann
pPred p0 = case p0 of
  PTrue -> "true"
  PFalse -> "false"
  PVar x -> pName x
  PInt c -> pretty c
  PFun f ps -> pName f <> tupled (map pPred ps)
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
  deriving (Eq, Show, Read)

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
