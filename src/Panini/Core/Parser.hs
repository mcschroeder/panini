{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Panini.Core.Parser
  ( parseExpr
  , parseConstraint
  ) where

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Bifunctor
import Data.Char
import Data.List (foldl')
import Data.Set qualified as Set
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void
import Panini.Core.Syntax
import Prelude
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Printf

-------------------------------------------------------------------------------

parseExpr :: FilePath -> Text -> Either String Expr
parseExpr = parseA expr

parseConstraint :: FilePath -> Text -> Either String Con
parseConstraint = parseA constraint

parseA :: Parser a -> FilePath -> Text -> Either String a
parseA p fp = first errorBundlePretty . parse (p <* eof) fp

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
  , "forall"
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

  , try $ Ass <$ keyword "assume" <*> name
              <* symbol ":" <*> type_
              <* keyword "in" <*> expr
  
  , try $ If <$ keyword "if" <*> value 
             <* keyword "then" <*> expr 
             <* keyword "else" <*> expr
  
  , try $ Rec <$ keyword "rec" <*> name 
              <* symbol ":" <*> type_ 
              <* symbol "=" <*> expr 
              <* keyword "in" <*> expr
  
  , try $ Let <$ keyword "let" <*> name 
              <* symbol "=" <*> expr 
              <* keyword "in" <*> expr
  
  , try $ Lam <$ lambda <*> name 
              <* symbol "." <*> expr
  
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
  choice
    [ notFollowedBy arrow *> pure t1
    , foldl' (TFun x) t1 <$> (arrow *> some type_)
    ]

type1 :: Parser (Name, Type)
type1 = choice
  [ try nested       -- (t)
  , try namedNested  -- x:(t)
  , try namedReft    -- x:{v:b|r}
  , try reft         -- {v:b|r}
  , try namedBase    -- x:b
  , try base         -- b
  ]
 where
  nested      = parens $ (,) <$> pure dummyName <*> type_
  namedNested = (,) <$> name <* symbol ":" <*> parens type_
  namedReft   = (,) <$> name <* symbol ":" <*> (snd <$> reft)
  reft = do
    t@(TBase v _ _) <- braces $ TBase <$> name <* symbol ":" <*> baseType 
                                               <* symbol "|" <*> refinement
    pure (v,t)
  namedBase = do
    (x,b) <- (,) <$> name <* symbol ":" <*> baseType
    pure (x, TBase x b (Known pTrue))
  base = do
    b <- baseType
    pure (dummyName, TBase dummyName b (Known pTrue))

baseType :: Parser Base
baseType = choice
  [ TUnit <$ keyword "unit"
  , TBool <$ keyword "bool"
  , TInt <$ keyword "int"
  , TString <$ keyword "string"
  ] <?> "base type"

refinement :: Parser Reft
refinement = (Unknown <$ symbol "?" <|> Known <$> predicate) <?> "refinement"

-------------------------------------------------------------------------------

-- | Parses a `Predicate`.
predicate :: Parser Pred
predicate = makeExprParser predTerm (predOpsBase ++ predOpsLogic)

-- | Parses a `Predicate` that does not contain logical connectives. 
-- We need this to disambiguate within the `Constraint` parser.
predicateNoLogic :: Parser Pred
predicateNoLogic = makeExprParser predTerm predOpsBase

predTerm :: Parser Pred
predTerm = choice
  [ parens predicate
  , try $ PVal <$> value <* notFollowedBy "("
  , PFun <$> name <*> parens (sepBy1 predicate ",")
  ]

predOpsBase :: [[Operator Parser Pred]]
predOpsBase =
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
  ]

predOpsLogic :: [[Operator Parser Pred]]
predOpsLogic =
  [ [ infixR symConj PConj
    ]
  , [ infixR symDisj PDisj
    ]
  , [ infixN symImpl PImpl
    , infixN symIff PIff
    ]
  ]

prefix :: Functor m => m b -> (a -> a) -> Operator m a
prefix p f = Prefix (f <$ p)

infixL, infixN, infixR :: Functor m => m b -> (a -> a -> a) -> Operator m a
infixL p f = InfixL (f <$ p)
infixN p f = InfixN (f <$ p)
infixR p f = InfixR (f <$ p)

-- | Parses an operator symbol even if it overlaps with another operator symbol.
op :: Text -> Parser Text
op n = (lexeme . try) (string n <* notFollowedBy (satisfy isOpSym))
 where
   isOpSym c = c `elem` ['=', '<', '>', '/', '\\']

-------------------------------------------------------------------------------

constraint :: Parser Con
constraint = makeExprParser conTerm conOps  

conTerm :: Parser Con
conTerm = choice
  [ try $ CPred <$> embeddedPredicate
  , CAll <$ symAll <*> name
         <* symbol ":" <*> baseType 
         <* symbol "." <*> embeddedPredicate
         <* symImpl <*> constraint  
  ]
 where
  embeddedPredicate = try (parens predicate) <|> predicateNoLogic

conOps :: [[Operator Parser Con]]
conOps = [[InfixR (CConj <$ symConj)]]

-------------------------------------------------------------------------------

-- | Parses an arrow.
arrow :: Parser ()
arrow = void $ symbol "->" <|> symbol "→"

-- | Parses a lambda.
lambda :: Parser ()
lambda = void $ symbol "\\" <|> symbol "λ"

-- | Parses a predicate negation symbol.
symNot :: Parser ()
symNot = void $ op "~" <|> symbol "¬"

-- | Parses a disequality symbol.
symNeq :: Parser ()
symNeq = void $ op "/=" <|> symbol "≠"

-- | Parsers a less-than-or-equal symbol.
symLeq :: Parser ()
symLeq = void $ op "<=" <|> symbol "≤"

-- | Parsers a greater-than-or-equal symbol.
symGeq :: Parser ()
symGeq = void $ op ">=" <|> symbol "≥"

-- | Parses a conjunction symbol.
symConj :: Parser ()
symConj = void $ op "/\\" <|> symbol "∧"

-- | Parses a disjunction symbol.
symDisj :: Parser ()
symDisj = void $ op "\\/" <|> symbol "∨"

-- | Parses an implication symbol.
symImpl :: Parser ()
symImpl = void $ op "==>" <|> symbol "⇒"

-- | Parses an if-and-only-if symbol.
symIff :: Parser ()
symIff = void $ op "<=>" <|> symbol "⇔"

-- | Parses a forall symbol/keyword.
symAll :: Parser ()
symAll = void $ keyword "forall" <|> symbol "∀"
