{-# LANGUAGE OverloadedStrings #-}

module Language.Panini.Printer (prettyPrint, prettyPut) where

import Data.Text (Text)
import Language.Panini.Syntax
import Prettyprinter
import Prettyprinter.Render.Text
import System.Console.ANSI
import Data.Text.IO qualified as Text

-------------------------------------------------------------------------------

prettyPrint :: Int -> Expr -> Text
prettyPrint w = renderStrict . layoutSmart opts . pExpr
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

pExpr :: Expr -> Doc ()
pExpr = \case
  Val x -> pValue x
  App e x -> pExpr e <+> pValue x    
  Lam x e -> nest 2 $ "\\" <> pName x <> "." <\> pExpr e
  Ann e t -> pExpr e <+> ":" <+> pType t
  Let x e1 e2 -> "let" <+> pName x <+> "=" <+> pExpr e1 <+> "in" <\> pExpr e2
  Rec x t e1 e2 -> "rec" <+> pName x <+> ":" <+> pType t <\> "=" <+> pExpr e1 <\> "in" <\> pExpr e2
  If x e1 e2 -> "if" <+> pValue x <+> nest 2 ("then" <\> pExpr e1) <\> nest 2 ("else" <\> pExpr e2)

pValue :: Value -> Doc ()
pValue = \case
  Unit -> "unit"
  B True -> "true"
  B False -> "false"
  I c -> pretty c
  S t -> viaShow t
  Var x -> pName x

-------------------------------------------------------------------------------

isPi :: Type -> Bool
isPi (Pi _ _ _) = True
isPi _ = False

isT :: Refinement -> Bool
isT (Known PTrue) = True
isT _ = False

arr :: Doc ann -> Doc ann -> Doc ann
arr a b = a <+> "->" <+> b

col :: Name -> Doc ann -> Doc ann
col x a = pName x <> ":" <> a

pType :: Type -> Doc ()
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

pBaseTy :: BaseType -> Doc ()
pBaseTy = \case
  TyUnit -> "unit"
  TyBool -> "bool"
  TyInt -> "int"
  TyString -> "string"

-------------------------------------------------------------------------------

pReft :: Refinement -> Doc ()
pReft = \case
  Unknown -> "?"
  Known p -> pPred p

pPred :: Pred -> Doc ann
pPred p0 = case p0 of
  PTrue -> "true"
  PFalse -> "false"
  PVar x -> pName x
  PInt c -> pretty c
  PFun f ps -> pName f <> tupled (map pPred ps)
  PNot p1 -> prettyUnary p0 p1 "~"
  PBin Mul p1 p2 -> prettyOp p0 p1 p2 "*"
  PBin Div p1 p2 -> prettyOp p0 p1 p2 "/"
  PBin Add p1 p2 -> prettyOp p0 p1 p2 "+"
  PBin Sub p1 p2 -> prettyOp p0 p1 p2 "-"
  PRel Neq p1 p2 -> prettyOp p0 p1 p2 "/="
  PRel Eq p1 p2  -> prettyOp p0 p1 p2 "="
  PRel Leq p1 p2 -> prettyOp p0 p1 p2 "<="
  PRel Lt p1 p2  -> prettyOp p0 p1 p2 "<"
  PRel Geq p1 p2 -> prettyOp p0 p1 p2 ">="
  PRel Gt p1 p2  -> prettyOp p0 p1 p2 ">"
  PConj p1 p2    -> prettyOp p0 p1 p2 "/\\"
  PDisj p1 p2    -> prettyOp p0 p1 p2 "\\/"
  PImpl p1 p2    -> prettyOp p0 p1 p2 "==>"
  PIff p1 p2     -> prettyOp p0 p1 p2 "<=>"

prettyUnary :: Pred -> Pred -> Doc ann -> Doc ann
prettyUnary o l docO = docO <> parensIf (pO > pL) (pPred l)
 where
   (pO, _) = fixity o
   (pL, _) = fixity l

prettyOp :: Pred -> Pred -> Pred -> Doc ann -> Doc ann
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
