{-# LANGUAGE OverloadedStrings #-}

module Language.Panini.Printer (prettyPrint, prettyPut) where

import Data.Text (Text)
import Language.Panini.Syntax
import Prettyprinter
import Prettyprinter.Render.Text
import System.Console.ANSI
import Data.Text.IO qualified as Text

ex1 :: Expr
ex1 = Rec (Name "empty") (Pi (Name "s") (Base (Name "s") TyString (Known PTrue)) (Base (Name "b") TyBool (Known (POp Eq (PUf (Name "length") [PVar (Name "s")]) (PInt 0))))) (Val (Var (Name "bot"))) (Val Unit)

ex2 :: Expr
ex2 = Rec (Name "empty") (Pi (Name "s") (Base (Name "s") TyString (Known PTrue)) (Base (Name "b") TyBool (Known (POp Eq (PUf (Name "length") [PVar (Name "s")]) (PInt 0))))) (Lam (Name "x") (Let (Name "n") (App (Val (Var (Name "length"))) (Var (Name "x"))) (App (App (Val (Var (Name "eq_n"))) (I 0)) (Var (Name "n"))))) (Val Unit)

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
  S t -> dquotes $ pretty t   --- TODO: escape string
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

data Assoc = Prefix | InfixL | InfixN | InfixR
type Fixity = (Assoc, Int)

parensL :: Maybe Fixity -> Maybe Fixity -> Doc ann -> Doc ann
parensL (Just (_     , m)) (Just (_, n)) d | m > n  = parens d
parensL (Just (InfixR, m)) (Just (_, n)) d | m == n = parens d
parensL _           _                    d          = d

parensR :: Maybe Fixity -> Maybe Fixity -> Doc ann -> Doc ann
parensR (Just (_     , m)) (Just (_, n)) d | m > n  = parens d
parensR (Just (InfixL, m)) (Just (_, n)) d | m == n = parens d
parensR _           _                    d          = d

binOp :: (Maybe Fixity, Doc ann) -- ^ left argument 
      -> (Maybe Fixity, Doc ann) -- ^ operator
      -> (Maybe Fixity, Doc ann) -- ^ right argument
      -> Doc ann
binOp (f1, d1) (f0, d0) (f2, d2) = parensL f0 f1 d1 <+> d0 <+> parensR f0 f2 d2

predFixity :: Pred -> Maybe Fixity
predFixity = \case
  PNeg _ -> Just (Prefix, 5)
  POp Mul _ _ -> Just (InfixL, 4)
  POp Div _ _ -> Just (InfixL, 4)
  POp Add _ _ -> Just (InfixL, 3)
  POp Sub _ _ -> Just (InfixL, 3)
  POp _ _ _ -> Just (InfixN, 2)
  PConj _ _ -> Just (InfixN, 1)
  PDisj _ _ -> Just (InfixN, 1)
  _ -> Nothing

-- TODO: explicit parenthesis get lost! (here or in parser?)

pPred :: Pred -> Doc ()
pPred p0 = case p0 of
  PTrue -> "true"
  PFalse -> "false"
  PVar x -> pName x
  PInt c -> pretty c
  
  PUf f ps -> pName f <> parens (hsep $ punctuate "," $ map pPred ps)
  
  PNeg p
    | Just _ <- predFixity p -> "~" <> parens (pPred p)
    | otherwise              -> "~" <>         pPred p
               
  PConj p1 p2 -> binOp (predFixity p1, pPred p1) (predFixity p0, "&") (predFixity p2, pPred p2)
  PDisj p1 p2 -> binOp (predFixity p1, pPred p1) (predFixity p0, "|") (predFixity p2, pPred p2)
  POp o p1 p2 -> binOp (predFixity p1, pPred p1) (predFixity p0, pOp o) (predFixity p2, pPred p2)
  
pOp :: POp -> Doc ()
pOp = \case
  Eq  -> "="
  Neq -> "/="
  Leq -> "<="
  Geq -> ">="
  Lt  -> "<"
  Gt  -> ">"
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Div -> "/"
