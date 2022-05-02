{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Panini.Parser
  ( parseProg
  , parseDecl
  , parseTerm
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
import Panini.Provenance
import Panini.Error
import Panini.Syntax
import Prelude
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Printf
import Data.List.NonEmpty qualified as NE
import Data.Maybe

-------------------------------------------------------------------------------

parseProg :: FilePath -> Text -> Either Error Prog
parseProg = parseA (many decl)

parseDecl :: FilePath -> Text -> Either Error Decl
parseDecl = parseA decl

parseTerm :: FilePath -> Text -> Either Error Term
parseTerm = parseA term

parseA :: Parser a -> FilePath -> Text -> Either Error a
parseA p fp = first transformErrorBundle . parse (p <* eof) fp

transformErrorBundle :: ParseErrorBundle Text Void -> Error
transformErrorBundle b = ParserError (FromSource loc (Just offLine)) errMsg
  where
    firstError    = NE.head $ b.bundleErrors
    errMsg        = Text.pack $ parseErrorTextPretty firstError
    offLine       = Text.pack $ fromMaybe "" msline
    (msline, pst) = reachOffset (errorOffset firstError) b.bundlePosState

    loc = SrcLoc fn (l1,c1) (l2,c2)
    fn = pst.pstateSourcePos.sourceName
    l1 = unPos pst.pstateSourcePos.sourceLine
    c1 = unPos pst.pstateSourcePos.sourceColumn
    l2 = unPos pst.pstateSourcePos.sourceLine
    c2 = unPos pst.pstateSourcePos.sourceColumn + errLen

    errLen = case firstError of
      TrivialError _ (Just (Tokens ts)) _ -> length ts 
      _                                   -> 1

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
symbol :: Text -> Parser ()
symbol = void . L.symbol whitespace

-- | Parses a keyword.
--
-- >>> parseTest (keyword "a") "ab"
-- unexpected 'b'
keyword :: Text -> Parser ()
keyword kw = void $ lexeme (string kw <* notFollowedBy identChar)

-- | Parses something between parentheses.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

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
  , "assume", "define"
  ]

-- | Report a parse error at the given offset.
failWithOffset :: Int -> String -> Parser a
failWithOffset o = parseError . FancyError o . Set.singleton . ErrorFail

-- | Construct a `FromSource` provenance out of beginning and end `SourcePos`.
mkPV :: SourcePos -> SourcePos -> PV
mkPV b e = FromSource (SrcLoc b.sourceName b' e') Nothing
  where
    b' = (unPos b.sourceLine, unPos b.sourceColumn)
    e' = (unPos e.sourceLine, unPos e.sourceColumn)

-- | Return the result of a parser together with a `PV` of the consumed input.
withPV :: Parser a -> Parser (a, PV)
withPV p = do
  begin <- getSourcePos
  x <- p
  end <- getSourcePos
  return (x, mkPV begin end)

addPV :: Parser (PV -> a) -> Parser a
addPV p = do
  (a0, pv) <- withPV p
  return $ a0 pv

-------------------------------------------------------------------------------

name :: Parser Name
name = label "name" $ do
  o <- getOffset
  (ident, pv) <- withPV $ (:) <$> identBeginChar <*> many identChar
  whitespace
  if isReserved ident
    then failWithOffset o $ printf "unexpected keyword \"%s\"" ident
    else do      
      pure $ Name (Text.pack ident) pv

-------------------------------------------------------------------------------

decl :: Parser Decl
decl = choice [assume, define]

assume :: Parser Decl
assume = do
  keyword "assume"
  x <- name
  symbol ":"
  t <- type_
  return $ Assume x t

define :: Parser Decl
define = do
  keyword "define"
  x <- name
  symbol "="
  e <- term
  return $ Define x e

-------------------------------------------------------------------------------

term :: Parser Term
term = do
  e1 <- term1
  e <- (foldl' App e1 <$> some (try value)) <|> pure e1
  (Ann e <$ symbol ":" <*> type_) <|> pure e

term1 :: Parser Term
term1 = choice
  [ try $ parens term

  , try $ If <$ keyword "if" <*> value 
             <* keyword "then" <*> term 
             <* keyword "else" <*> term
  
  , try $ Rec <$ keyword "rec" <*> name 
              <* symbol ":" <*> type_ 
              <* symbol "=" <*> term 
              <* keyword "in" <*> term
  
  , try $ Let <$ keyword "let" <*> name 
              <* symbol "=" <*> term 
              <* keyword "in" <*> term
  
  , try $ Lam <$ lambda <*> name 
              <* symbol "." <*> term
  
  , Val <$> value
  ]

value :: Parser Value
value = label "value" $ choice
  [ (addPV $ U <$ string "unit") <* whitespace
  , boolLit
  , intLit
  , stringLit
  , V <$> name
  ]
  where
    boolLit = do
      (x, pv) <- withPV $ True <$ string "true" <|> False <$ string "false"
      whitespace
      return $ B x pv
    
    intLit = label "integer" $ do
      (x, pv) <- withPV $ L.signed empty L.decimal
      whitespace
      return $ I x pv
    
    stringLit = label "string" $ do
      begin <- getSourcePos
      x <- Text.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))
      end <- getSourcePos 
      whitespace
      return $ S x (mkPV begin end)

-------------------------------------------------------------------------------

type_ :: Parser Type
type_ = do
  begin <- getSourcePos
  (x, t1) <- type1
  choice 
    [ notFollowedBy arrow *> pure t1
    , do
        t2 <- arrow *> type_
        end <- getSourcePos
        return $ TFun x t1 t2 (mkPV begin end)
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
  nested = parens $ do
    t <- type_
    case t of
      TBase x _ _ _-> pure (x, t)
      TFun _ _ _ _ -> pure (dummyName, t)

  namedNested = (,) <$> name <* symbol ":" <*> parens type_
  namedReft   = (,) <$> name <* symbol ":" <*> (snd <$> reft)
  reft = do
    begin <- getSourcePos
    symbol "{"
    v <- name
    symbol ":"
    b <- baseType
    symbol "|"
    r <- refinement
    void "}"
    end <- getSourcePos
    whitespace
    pure (v, TBase v b r (mkPV begin end))
  namedBase = do
    begin <- getSourcePos
    x <- name
    symbol ":"
    b <- baseType
    end <- getSourcePos
    pure (x, TBase x b (Known pTrue) (mkPV begin end))
  base = do
    (b, pv) <- withPV baseType
    pure (dummyName, TBase dummyName b (Known pTrue) pv)

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
predicate = makeExprParser predTerm predOps

predTerm :: Parser Pred
predTerm = choice
  [ parens predicate
  , predAll
  , try $ PVal <$> value <* notFollowedBy "("
  , PFun <$> name <*> parens (sepBy1 predicate ",")
  ]
  where
    predAll = 
      PAll <$ symAll <*> name
           <* symbol ":" <*> baseType 
           <* symbol "." <*> predicate
           <* symImpl <*> predicate

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
op :: Text -> Parser ()
op n = (void . lexeme . try) (string n <* notFollowedBy (satisfy isOpSym))
 where
   isOpSym c = c `elem` ['=', '<', '>', '/', '\\']

-------------------------------------------------------------------------------

-- | Parses an arrow.
arrow :: Parser ()
arrow = symbol "->" <|> symbol "→"

-- | Parses a lambda.
lambda :: Parser ()
lambda = symbol "\\" <|> symbol "λ"

-- | Parses a predicate negation symbol.
symNot :: Parser ()
symNot = op "~" <|> symbol "¬"

-- | Parses a disequality symbol.
symNeq :: Parser ()
symNeq = op "/=" <|> symbol "≠"

-- | Parsers a less-than-or-equal symbol.
symLeq :: Parser ()
symLeq = op "<=" <|> symbol "≤"

-- | Parsers a greater-than-or-equal symbol.
symGeq :: Parser ()
symGeq = op ">=" <|> symbol "≥"

-- | Parses a conjunction symbol.
symConj :: Parser ()
symConj = op "/\\" <|> symbol "∧"

-- | Parses a disjunction symbol.
symDisj :: Parser ()
symDisj = op "\\/" <|> symbol "∨"

-- | Parses an implication symbol.
symImpl :: Parser ()
symImpl = op "==>" <|> symbol "⇒"

-- | Parses an if-and-only-if symbol.
symIff :: Parser ()
symIff = op "<=>" <|> symbol "⇔"

-- | Parses a forall symbol/keyword.
symAll :: Parser ()
symAll = keyword "forall" <|> symbol "∀"
