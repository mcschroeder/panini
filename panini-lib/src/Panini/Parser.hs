{-# LANGUAGE TypeFamilies #-}

module Panini.Parser
  ( parseProgram
  , parseStatement
  , parseTerm
  , parseType
  , parseRel
  , parseConstraint
  , Error(..)
  ) where

import Control.Monad
import Control.Monad.Combinators.Expr
import Control.Monad.Trans.Class
import Control.Monad.Trans.State qualified as MT
import Data.Bifunctor
import Data.Char
import Data.List (foldl')
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Set qualified as Set
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void
import Panini.Diagnostic
import Panini.Pretty (pretty)
import Panini.Provenance
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude
import Regex.POSIX.ERE qualified as ERE
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Printf

-------------------------------------------------------------------------------

parseProgram :: FilePath -> Text -> Either Error Program
parseProgram = parseA $ whitespace >> many statement

parseStatement :: FilePath -> Text -> Either Error Statement
parseStatement = parseA statement

parseTerm :: FilePath -> Text -> Either Error Term
parseTerm = parseA term

parseType :: FilePath -> Text -> Either Error Type
parseType = parseA type_

parseRel :: FilePath -> Text -> Either Error Rel
parseRel = parseA rel

parseConstraint :: FilePath -> Text -> Either Error Con
parseConstraint = parseA constraint

parseA :: Parser a -> FilePath -> Text -> Either Error a
parseA p fp s = first transformErrorBundle $ MT.evalState (runParserT (p <* eof) fp s) mempty

-------------------------------------------------------------------------------

data Error = Error Text PV

instance HasProvenance Error where
  getPV (Error _ pv) = pv

instance Diagnostic Error where
  diagnosticMessage (Error e _) = pretty e

transformErrorBundle :: ParseErrorBundle Text Void -> Error
transformErrorBundle b = Error errorMessage provenance
  where
    provenance    = FromSource loc (Just offLine)
    firstError    = NE.head $ b.bundleErrors
    errorMessage  = Text.pack $ parseErrorTextPretty firstError
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

type VarCtx = [(Name,Base)]

withVar :: Name -> Base -> Parser a -> Parser a
withVar x b p = do
  lift $ MT.modify' ((x,b):)
  a <- p
  lift $ MT.modify' tail
  return a

lookupVar :: Name -> Parser Base
lookupVar x = lift (MT.gets (lookup x)) >>= \case
  Nothing -> fail $ "unknown refinement variable: " ++ show (pretty x)
  Just b -> return b

-------------------------------------------------------------------------------

type Parser = ParsecT Void Text (MT.State VarCtx)

-- | Consumes white space, including newlines. Skips comments.
whitespace :: Parser ()
whitespace = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "--"
    blockComment = L.skipBlockCommentNested "{-" "-}"

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
  <|> try (single '.' <* notFollowedBy (single '.'))

-- | Returns `True` if the given string is a reserved keyword.
isReserved :: (IsString s, Eq s) => s -> Bool
isReserved = flip elem
  [ "if", "then", "else"
  , "rec", "let", "in"
  , "true", "false", "unit"
  , "bool", "int", "string"
  , "forall"
  , "assume", "define", "import"
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

getEndSourcePosFromPV :: PV -> Maybe SourcePos
getEndSourcePosFromPV = \case
  FromSource (SrcLoc n _ (l,c)) _ -> Just $ SourcePos n (mkPos l) (mkPos c)
  Derived pv _ -> getEndSourcePosFromPV pv
  NoPV -> Nothing

-- | Return the result of a parser together with a `PV` of the consumed input.
withPV :: Parser a -> Parser (a, PV)
withPV p = do
  begin <- getSourcePos
  x <- p
  end <- getSourcePos
  return (x, mkPV begin end)

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

statement :: Parser Statement
statement = choice 
  [ import_
  , try $ Assume <$> name <* symbol ":" <*> type_ <* whitespace
  , Define <$> name <* symbol "=" <*> term <* whitespace
  ]

import_ :: Parser Statement
import_ = do
  keyword "import"
  (m, pv) <- withPV $ some $ satisfy (\x -> isAlphaNum x || x == '_' || x == '/' || x == '.')
  whitespace
  return $ Import m pv

-------------------------------------------------------------------------------

atom :: Parser Atom
atom = choice
  [ Con <$> value
  , Var <$> name 
  ]

term :: Parser Term
term = do
  e1 <- term1
  let mkApp e x = App e x NoPV  -- TODO: figure out the provenance here
  let appVal = atom <* notFollowedBy (symbol ":" <|> symbol "=")
  (foldl' mkApp e1 <$> some (try appVal)) <|> pure e1

term1 :: Parser Term
term1 = choice
  [ try $ parens term

  , try $ do
      begin <- getSourcePos
      p <- keyword "if" *> atom
      e1 <- keyword "then" *> term
      e2 <- keyword "else" *> term
      end <- maybe getSourcePos pure $ getEndSourcePosFromPV $ getPV e2
      return $ If p e1 e2 $ mkPV begin end
  
  , try $ do
      begin <- getSourcePos
      x <- keyword "rec" *> name
      t <- symbol ":" *> type_
      e1 <- symbol "=" *> term
      e2 <- keyword "in" *> term
      end <- maybe getSourcePos pure $ getEndSourcePosFromPV $ getPV e2
      return $ Rec x t e1 e2 $ mkPV begin end
  
  , try $ do
      begin <- getSourcePos
      x <- keyword "let" *> name
      e1 <- symbol "=" *> term
      e2 <- keyword "in" *> term
      end <- maybe getSourcePos pure $ getEndSourcePosFromPV $ getPV e2
      return $ Let x e1 e2 $ mkPV begin end

  , try $ do
      begin <- getSourcePos
      o <- getOffset
      lambda
      (x0,t) <- type1
      case x0 of
        Nothing -> failWithOffset o "missing binder"
        Just x -> do
          symbol "."
          e <- case t of
            TBase _ b _ _ -> withVar x b term
            TFun _ _ _ _ -> term
          end <- maybe getSourcePos pure $ getEndSourcePosFromPV $ getPV e
          return $ Lam x t e $ mkPV begin end

  , Val <$> atom
  ]

value :: Parser Value
value = label "value" $ choice
  [ unitLit
  , boolLit
  , intLit
  , charLit
  , stringLit
  ]
  where
    unitLit = do
      (_, pv) <- withPV $ U <$ string "unit"
      whitespace
      return $ U pv

    boolLit = do
      (x, pv) <- withPV $ True <$ string "true" <|> False <$ string "false"
      whitespace
      return $ B x pv
    
    intLit = label "integer" $ do
      (x, pv) <- withPV $ L.signed whitespace L.decimal
      whitespace
      return $ I x pv
    
    charLit = label "character" $ do
      (x,pv) <- withPV $ char '\'' *> L.charLiteral <* char '\''
      whitespace
      return $ C x pv
    
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
        t2 <- case t1 of
          TBase x1 b1 _ _ -> withVar (fromMaybe x1 x) b1 $ arrow *> type_
          TFun _ _ _ _  -> arrow *> type_
        end <- getSourcePos
        return $ TFun (fromMaybe dummyName x) t1 t2 (mkPV begin end)
    ]

type1 :: Parser (Maybe Name, Type)
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
      TBase x _ _ _-> pure (Just x, t)
      TFun _ _ _ _ -> pure (Nothing, t)

  namedNested = (,) <$> (Just <$> name) <* symbol ":" <*> parens type_
  namedReft   = (,) <$> (Just <$> name) <* symbol ":" <*> (snd <$> reft)  
  reft = do
    begin <- getSourcePos
    symbol "{"
    v <- name
    symbol ":"
    b <- baseType
    symbol "|"
    r <- withVar v b refinement
    void "}"    
    end <- getSourcePos
    whitespace    
    pure (Just v, TBase v b r (mkPV begin end))
  
  namedBase = do
    begin <- getSourcePos
    x <- name
    symbol ":"
    b <- baseType
    end <- getSourcePos
    pure (Just x, TBase x b (Known PTrue) (mkPV begin end))
  
  base = do
    (b, pv) <- withPV baseType
    pure (Nothing, TBase dummyName b (Known PTrue) pv)

baseType :: Parser Base
baseType = choice
  [ TUnit <$ (keyword "unit" <|> keyword "𝟙")
  , TBool <$ (keyword "bool" <|> keyword "𝔹")
  , TInt <$ (keyword "int" <|> keyword "ℤ")
  , TChar <$ (keyword "char" <|> keyword "ℂ𝕙")
  , TString <$ (keyword "string" <|> keyword "𝕊")
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
  , try regRel
  , try (PRel <$> rel)
  , PTrue <$ keyword "true"
  , PFalse <$ keyword "false"
  ]

predOps :: [[Operator Parser Pred]]
predOps =
  [ [ prefix symNot PNot
    ]
  , [ infixR symConj mkAnd
    ]
  , [ infixR symDisj mkOr
    ]
  , [ infixN symImpl PImpl
    , infixN symIff PIff
    ]
  ]

mkAnd :: Pred -> Pred -> Pred
mkAnd (PAnd ps) (PAnd qs) = PAnd (ps ++ qs)
mkAnd (PAnd ps) q         = PAnd (ps ++ [q])
mkAnd p         (PAnd qs) = PAnd (p:qs)
mkAnd p         q         = PAnd [p,q]

mkOr :: Pred -> Pred -> Pred
mkOr (POr ps) (POr qs) = POr (ps ++ qs)
mkOr (POr ps) q        = POr (ps ++ [q])
mkOr p        (POr qs) = POr (p:qs)
mkOr p        q        = POr [p,q]

regRel :: Parser Pred
regRel = do
  e1 <- pexpr
  ctor <- ((:∈:) <$ symIn) <|> ((:∉:) <$ symNotIn)
  e2 <- EReg <$> ERE.ere
  return (PRel (ctor e1 e2))

rel :: Parser Rel
rel = do
  e1 <- pexpr
  ctor <- relation
  e2 <- pexpr
  return (ctor e1 e2)

relation :: Parser (Expr -> Expr -> Rel)
relation = choice
  [ (:≠:) <$ symNe
  , (:=:) <$ op "="
  , (:≤:) <$ symLe
  , (:<:) <$ op "<"
  , (:≥:) <$ symGe
  , (:>:) <$ op ">"
  --, (:∈:) <$ symIn
  --, (:∉:) <$ symNotIn
  ]

pexpr :: Parser Expr
pexpr = makeExprParser pexprTerm pexprOps

pexprTerm :: Parser Expr
pexprTerm = choice
  [ try $ EStrLen <$ symbol "|" <*> pexpr <* symbol "|"
  , try $ ECon <$> value <* notFollowedBy "("
  , try $ do
      x <- name <* notFollowedBy "("
      b <- lookupVar x
      return $ EVar x b
  , EFun <$> name <*> parens (sepBy1 pexpr ",")
  ]

pexprOps :: [[Operator Parser Expr]]
pexprOps =
  [ [ Postfix opSubStr ]
  , [ prefix symNot ENot ]
  , [ infixL (op "*") ((:*:))
    ]
  , [ infixL (op "+") ((:+:))
    , infixL (op "-") ((:-:))
    ]
  , [ infixR (op "++") EStrConc ]  
  ]

opSubStr :: Parser (Expr -> Expr)
opSubStr = foldr1 (flip (.)) <$> some subscripts
  where
    subscripts = do
      symbol "["
      i <- pexpr
      jm <- optional $ symbol ".." >> pexpr
      symbol "]"
      case jm of
        Just j -> return $ \s -> EStrSub s i j
        Nothing -> return $ \s -> EStrAt s i

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
   isOpSym c = c `elem` ['=', '<', '>', '/', '\\', '+']

-------------------------------------------------------------------------------

constraint :: Parser Con
constraint = choice
  [ call
  , CAnd <$> parens constraint <* symConj <*> constraint
  , CHead <$> predicate
  ]

-- TODO: this 
call :: Parser Con
call = do
  symAll
  x <- name
  symbol ":"
  b <- baseType
  symbol "."
  p <- parens predicate
  symImpl
  c <- constraint
  return $ CAll x b p c
  
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
symNe :: Parser ()
symNe = op "/=" <|> symbol "≠"

-- | Parsers a less-than-or-equal symbol.
symLe :: Parser ()
symLe = op "<=" <|> symbol "≤"

-- | Parsers a greater-than-or-equal symbol.
symGe :: Parser ()
symGe = op ">=" <|> symbol "≥"

-- | Parses a conjunction symbol.
symConj :: Parser ()
symConj = op "/\\" <|> symbol "∧"

-- | Parses a disjunction symbol.
symDisj :: Parser ()
symDisj = op "\\/" <|> symbol "∨"

-- | Parses an implication symbol.
symImpl :: Parser ()
symImpl = op "==>" <|> symbol "⇒" <|> symbol "⟹"

-- | Parses an if-and-only-if symbol.
symIff :: Parser ()
symIff = op "<=>" <|> symbol "⇔" <|> symbol "⟺"

symAll :: Parser ()
symAll = op "forall" <|> symbol "∀"

symIn :: Parser ()
symIn = op "\\in" <|> symbol "∈"

symNotIn :: Parser ()
symNotIn = op "\\notin" <|> symbol "∉"

-- symNi :: Parser ()
-- symNi = op "\\ni" <|> symbol "∋"

-- symNotNi :: Parser ()
-- symNotNi = op "\\notni" <|> symbol "∌"
