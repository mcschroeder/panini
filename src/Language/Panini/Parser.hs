{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Panini.Parser where

import Control.Monad
import Data.Char
import Data.List (foldl', foldl1')
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.String
import Data.Set qualified as Set
import Text.Printf

import Language.Panini.Syntax

-------------------------------------------------------------------------------

type Parser = Parsec Void Text

-- | Consumes white space, including newlines. No comments.
whitespace :: Parser ()
whitespace = L.space space1 empty empty

-- | Parses a lexeme and consumes all white space after the lexeme.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

-- | Parses a symbol and consumes all white space after the symbol.
--
-- >>> parseTest (symbol "a") "ab"
-- "a"
symbol :: Text -> Parser Text
symbol = L.symbol whitespace

-- | Parses a keyword.
--
-- >>> parseTest (keyword "a") "ab"
-- unexpected 'b'
keyword :: Text -> Parser Text
keyword kw = lexeme (string kw <* notFollowedBy identChar)

-- | Parses something between parentheses.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parses something between curly braces
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | Parses an arrow.
arrow :: Parser ()
arrow = void $ symbol "->"

-- | Parses a string literal.
stringLiteral :: Parser Text
stringLiteral = Text.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"')) <?> "string"

-- | Parses a (signed) integer literal.
integerLiteral :: Parser Integer
integerLiteral = L.signed whitespace (lexeme L.decimal) <?> "integer"

-- | Parses a boolean literal.
boolLiteral :: Parser Bool
boolLiteral = True <$ keyword "true" <|> False <$ keyword "false"

-- | Parses a character that is valid at the beginning of an identifier.
identBeginChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
identBeginChar = satisfy (\x -> isAlpha x || x == '_')

-- | Parses a character that is valid inside an identifier.
identChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
identChar = satisfy (\x -> isAlphaNum x || x == '_' || x == '\'')

-- | Returns `True` if the given string is a reserved keyword.
isReserved :: (IsString s, Eq s) => s -> Bool
isReserved = flip elem
  [ "if", "then", "else"
  , "rec", "let", "in"
  , "true", "false", "unit"
  , "bool", "int", "string"
  ]

-- | Report a parse error at the given offset.
failWithOffset :: Int -> String -> Parser a
failWithOffset o = parseError . FancyError o . Set.singleton . ErrorFail

-------------------------------------------------------------------------------

name :: Parser Name
name = label "name" $ do  
  o <- getOffset
  ident <- lexeme $ (:) <$> identBeginChar <*> many identChar  
  if isReserved ident
    then failWithOffset o $ printf "unexpected keyword \"%s\"" ident
    else pure $ Name $ Text.pack ident

-------------------------------------------------------------------------------

expr :: Parser Expr
expr = choice
  [ try $ If <$ keyword "if" <*> value <* keyword "then" <*> expr <* symbol "else" <*> expr
  , try $ Rec <$ keyword "rec" <*> name <* symbol ":" <*> type_ <* symbol "=" <*> expr <* symbol "in" <*> expr
  , Val <$> value
  ]

value :: Parser Value
value = (Con <$> constant <|> Var <$> name) <?> "value"

constant :: Parser Constant
constant = choice
  [ Unit <$ keyword "unit"
  , B <$> boolLiteral
  , I <$> integerLiteral
  , S <$> stringLiteral
  ]

-------------------------------------------------------------------------------

{-

b                   ===   Base dummyName b (Known (PBool True))
{x : b | r}         ===   Base x b r
x : t1 -> t2        ===   Pi x t1 t2
x : b -> t2         ===   Pi x (Base x b (Known (PBool True))) t2
{x : b | r} -> t2   ===   Pi x (Base x b r) t2
t1 -> t2            ===   Pi dummyName t1 t2



b
{x:b|r}
x:b
x:{y:b|r}
x:(t1 -> t2)

-}

type_ :: Parser Type
type_ = do
  (x, t1) <- type1
  try (foldl' (Pi x) t1 <$> (arrow *> some type_)) <|> pure t1

type1 :: Parser (Name, Type)
type1 = choice
  [ try nested       -- (t)
  , try namedNested  -- x : (t)
  , try namedReft    -- x : {y : b | r}
  , try reft         -- {x : b | r}
  , try namedBase    -- x : b
  , try base         -- b
  ]
 where
  nested      = parens $ (,) <$> pure dummyName <*> type_
  namedNested = (,) <$> name <* symbol ":" <*> parens type_
  namedReft   = (,) <$> name <* symbol ":" <*> (snd <$> reft)
  reft = do
    t@(Base x b r) <- braces $ Base <$> name <* symbol ":" <*> baseType <* symbol "|" <*> refinement
    pure (x,t)
  namedBase = do
    (x,b) <- (,) <$> name <* symbol ":" <*> baseType
    pure (x, Base x b (Known (PBool True)))
  base = do
    b <- baseType
    pure (dummyName, Base dummyName b (Known (PBool True)))

baseType :: Parser BaseType
baseType = choice
  [ TyUnit <$ keyword "unit"
  , TyBool <$ keyword "bool"
  , TyInt <$ keyword "int"
  , TyString <$ keyword "string"
  ]

refinement :: Parser Refinement
refinement = Unknown <$ symbol "?"

-- expr :: Parser Expr
-- expr = do
--   e0 <- expr0
--   e <- try (foldl' App e0 <$> some name) <|> pure e0
--   (Ann e <$ colon <*> type_) <|> pure e

-- expr0 :: Parser Expr
-- expr0 =
--   label "expression" $
--     choice
--       [ try (parens expr),
--         If <$ symbol "if" <*> name <* symbol "then" <*> expr <* symbol "else" <*> expr,
--         Let <$ symbol "let" <*> name <* symbol "=" <*> expr <* symbol "in" <*> expr,
--         Lam <$ lambda <*> name <* symbol "." <*> expr,
--         Con <$> constant <?> "constant",
--         Var <$> name <?> "variable"
--       ]

-- constant :: Parser Constant
-- constant =
--   choice
--     [ B <$> bool,
--       I <$> integerLiteral,
--       C <$> charLiteral,
--       S <$> stringLiteral
--     ]

-- -------------------------------------------------------------------------------

-- type_ :: Parser Type
-- type_ =
--   label "type" $
--     choice
--       [ try (parens (Pi <$> name <* colon <*> type0) <* arrow <*> type_),
--         try (Pi dummyName <$> type0 <* arrow <*> type_),
--         type0
--       ]

-- type0 :: Parser Type
-- type0 =
--   choice
--     [ try (parens type_)
-- --      Base <$> base <*> option Unknown refinement
--     ]

-- -- base :: Parser Base
-- -- base =
-- --   label "base type" $
-- --     choice
-- --       [ TyBool <$ symbol "bool",
-- --         TyInt <$ symbol "int",
-- --         TyChar <$ symbol "char",
-- --         TyString <$ symbol "string",
-- --         TyVar <$> name <?> "type variable"
-- --       ]

-- -------------------------------------------------------------------------------

-- refinement :: Parser Refinement
-- refinement = label "refinement" $ braces $ Unknown <$ symbol "?"

-- -------------------------------------------------------------------------------


-- integerLiteral :: Parser Integer
-- integerLiteral = label "integer" $ L.signed mempty (lexeme L.decimal)

-- charLiteral :: Parser Char
-- charLiteral = label "char" $ lexeme $ char '\'' >> L.charLiteral <* char '\''


-- lambda :: Parser ()
-- lambda = symbol "\\" <|> symbol "λ"

-- keyword :: Text -> Parser ()
-- keyword k = void $ lexeme $ string k <* notFollowedBy alphaNumChar







-- colon :: Parser ()
-- colon = symbol ":"

-- arrow :: Parser ()
-- arrow = symbol "->"

