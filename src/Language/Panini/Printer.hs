{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Panini.Printer where

import Data.Text (Text)
import Language.Panini.Syntax
import Prettyprinter
import Prettyprinter.Util (putDocW)

-- -------------------------------------------------------------------------------

-- data Ann = Keyword | Constant | Type | Refinement

-- printExpr :: Expr -> IO ()
-- printExpr = putDocW 80 . expr

-- -------------------------------------------------------------------------------

-- let_ :: Name -> Expr -> Doc Ann
-- let_ n e1 = keyword "let" <+> name n <+> keyword "=" <+> expr e1


-- expr :: Expr -> Doc Ann
-- expr = \case
--   Con c -> constant c
--   Var n -> name n
--   Let n e1 e2
--     | Lam {} <- e1 -> vsep [let_ n e1, nest 2 (keyword "in" <> line <> expr e2)]
--     | otherwise ->
--       sep
--         [ nest 2 (keyword "let" <+> name n <+> keyword "=" <+> expr e1),
--           nest 2 (keyword "in" <> line <> expr e2)
--         ]

--   Lam n e
--     | Lam {} <- e -> keyword "\\" <> name n <> dot <+> expr e
--     | otherwise -> nest 2 (keyword "\\" <> name n <> dot <> line <> expr e)
--   If n e1 e2 ->
--     sep
--       [ nest 2 (keyword "if" <+> name n),
--         nest 2 (keyword "then" <> line <> expr e1),
--         nest 2 (keyword "else" <> line <> expr e2)
--       ]
--   App e x -> expr e <+> name x
--   Ann e t -> expr e <+> ":" <+> type_ t

-- constant :: Constant -> Doc Ann
-- constant c = annotate Constant $ case c of
--   B True -> "true"
--   B False -> "false"
--   I i -> pretty i
--   C c -> squotes (pretty c)
--   S s -> dquotes (pretty s)

-- -------------------------------------------------------------------------------

-- type_ :: Type -> Doc Ann
-- type_ t = annotate Type $ case t of
--   Base b r -> base b <+> reft r
--   Pi n t1 t2 -> "(" <+> name n <+> ":" <+> type_ t1 <+> ")" <+> "->" <+> type_ t2

-- base :: Base -> Doc Ann
-- base t = annotate Type $ case t of
--   TyBool -> "bool"
--   TyInt -> "int"
--   TyChar -> "char"
--   TyString -> "string"
--   TyVar n -> name n

-- -------------------------------------------------------------------------------

-- reft :: Refinement -> Doc Ann
-- reft r = annotate Refinement $ case r of
--   Unknown -> "{" <+> "?" <+> "}"

-- -------------------------------------------------------------------------------

-- name :: Name -> Doc Ann
-- name (Name n) = pretty n

-- keyword :: Text -> Doc Ann
-- keyword = annotate Keyword . pretty
