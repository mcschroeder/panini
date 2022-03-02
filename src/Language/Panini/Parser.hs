{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Panini.Parser where

import Control.Monad
import Control.Monad.Combinators.Expr
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

-- | Parses a lambda.
lambda :: Parser ()
lambda = void $ symbol "\\"

-- | Parses a string literal.
stringLiteral :: Parser Text
stringLiteral = Text.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"')) <* whitespace <?> "string"

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
expr = do
  e1 <- expr1
  e <- (foldl' App e1 <$> some (try value)) <|> pure e1
  (Ann e <$ symbol ":" <*> type_) <|> pure e

expr1 :: Parser Expr
expr1 = choice
  [ try $ parens expr
  , try $ If <$ keyword "if" <*> value <* keyword "then" <*> expr <* symbol "else" <*> expr
  , try $ Rec <$ keyword "rec" <*> name <* symbol ":" <*> type_ <* symbol "=" <*> expr <* symbol "in" <*> expr
  , try $ Let <$ keyword "let" <*> name <* symbol "=" <*> expr <* symbol "in" <*> expr
  , try $ Lam <$ lambda <*> name <* symbol "." <*> expr
  , Val <$> value
  ]

value :: Parser Value
value = label "value" $ choice
  [ U <$ keyword "unit"
  , B <$> boolLiteral
  , I <$> integerLiteral
  , S <$> stringLiteral
  , V <$> name
  ]

-------------------------------------------------------------------------------

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
    pure (x, Base x b (Known PTrue))
  base = do
    b <- baseType
    pure (dummyName, Base dummyName b (Known PTrue))

baseType :: Parser BaseType
baseType = choice
  [ TyUnit <$ keyword "unit"
  , TyBool <$ keyword "bool"
  , TyInt <$ keyword "int"
  , TyString <$ keyword "string"
  ] <?> "base type"

refinement :: Parser Refinement
refinement = (Unknown <$ symbol "?" <|> Known <$> predicate) <?> "refinement"

-------------------------------------------------------------------------------

predicate :: Parser Pred
predicate = makeExprParser predTerm predOps

predTerm :: Parser Pred
predTerm = choice
  [ parens predicate
  , PTrue <$ symbol "true"
  , PFalse <$ symbol "false"
  , PInt <$> integerLiteral
  , try predFun
  , PVar <$> name
  ]

predFun :: Parser Pred
predFun = PFun <$> name <*> parens (sepBy1 predicate ",")

predOps :: [[Operator Parser Pred]]
predOps =
  [ [ prefix symNot PNot
    ]
  , [ infixL (op "*") (PBin Mul)
    , infixL (op "/") (PBin Div)
    ]
  , [ infixL (op "+") (PBin Add)
    , infixL (op "-") (PBin Sub)
    ]
  , [ infixN symNeq   (PRel Neq)
    , infixN (op "=") (PRel Eq)
    , infixN symLeq   (PRel Leq)
    , infixN (op "<") (PRel Lt)
    , infixN symGeq   (PRel Geq)
    , infixN (op ">") (PRel Gt)
    ]
  , [ infixR symConj PConj
    ]
  , [ infixR symDisj PDisj]
  , [ infixN symImpl PImpl
    , infixN symIff PIff
    ]
  ]
 where
  prefix p f = Prefix (f <$ p)
  infixL p f = InfixL (f <$ p)
  infixN p f = InfixN (f <$ p)
  infixR p f = InfixR (f <$ p)

-- | Parses an operator symbol even if it overlaps with another operator symbol.
op :: Text -> Parser Text
op n = (lexeme . try) (string n <* notFollowedBy (satisfy isOpSym))
 where
   isOpSym c = c `elem` ['=', '<', '>', '/', '\\']

-- | Parses a predicate negation symbol.
symNot :: Parser ()
symNot = void $ op "~"

-- | Parses a disequality symbol.
symNeq :: Parser ()
symNeq = void $ op "/="

-- | Parsers a less-than-or-equal symbol.
symLeq :: Parser ()
symLeq = void $ op "<="

-- | Parsers a greater-than-or-equal symbol.
symGeq :: Parser ()
symGeq = void $ op ">="

-- | Parses a conjunction symbol.
symConj :: Parser ()
symConj = void $ op "/\\"

-- | Parses a disjunction symbol.
symDisj :: Parser ()
symDisj = void $ op "\\/"

-- | Parses an implication symbol.
symImpl :: Parser ()
symImpl = void $ op "==>"

-- | Parses an if-and-only-if symbol..
symIff :: Parser ()
symIff = void $ op "<=>"
