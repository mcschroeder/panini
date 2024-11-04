{-| 
This module implements types and functions for handling POSIX Extended Regular
Expressions (EREs), in particular conversion to and from the 'Regex' type.

References:

  * Goyvaerts, Jan. 2021. "POSIX Basic and Extended Regular Expressions." Last
    updated August 24, 2021. https://www.regular-expressions.info/posix.html

  * IEEE and The Open Group. 2018. "Regular Expressions." Chap. 9 in The Open
    Group Base Specifications Issue 7 (IEEE Std 1003.1-2017).
    https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap09.html

  * Wikipedia. 2023. "Regular Expression." Accessed November 22, 2023.
    https://en.wikipedia.org/wiki/Regular_expression

-}
module Panini.Regex.POSIX.ERE where

import Control.Monad.Combinators.NonEmpty qualified as NE
import Data.Data (Data)
import Data.Hashable
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Void
import GHC.Generics
import Panini.Regex.POSIX.BE (BE)
import Panini.Regex.POSIX.BE qualified as BE
import Panini.Regex.CharSet qualified as CS
import Prelude hiding (exp, min)
import Panini.Regex.Type
import Panini.Panic
import Panini.Pretty
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- TODO: escaping
-- TODO: how to deal with ERE 'Per' excluding \NUL vs. Regex 'AnyChar' ?
-- TODO: figure out how to deal with anchors (should unanchored EREs convert to Σ*(r)Σ* ?)

------------------------------------------------------------------------------

-- | Extended Regular Expression (ERE) as defined by the POSIX standard.
data ERE
  = Alt (NonEmpty Con)  -- ^ alternation @x|y|z@
  deriving stock (Eq, Ord, Show, Read, Generic, Data)

-- | ERE concatenation.
data Con
  = Con (NonEmpty Exp)  -- ^ concatenation @xyz@
  deriving stock (Eq, Ord, Show, Read, Generic, Data)

-- | ERE (sub)expressions.
data Exp
  = Chr Char     -- ^ single ordinary character @a@
  | Per          -- ^ period @.@ (matches any character except @\NUL@)
  | Bra BE       -- ^ bracket expression @[abc]@, @[a-z]@, @[^abc]
  | Cir          -- ^ circumflex @^@ (matches beginning of string)
  | Dol          -- ^ dollar sign @$@ (matches end of string)
  | Grp ERE      -- ^ group @(x)@
  | Dup Exp Dup  -- ^ duplication @r*@, @r?@, @r{2,5}@
  deriving stock (Eq, Ord, Show, Read, Generic, Data)

-- | ERE duplication symbols.
data Dup
  = Ast          -- ^ asterisk @*@ (matches zero or more expressions)
  | Pls          -- ^ plus sign @+@ (matches one or more expressions)
  | Que          -- ^ question mark @?@ (matches zero or one expressions)
  | Exa Int      -- ^ exact number of occurrences @{m}@
  | Min Int      -- ^ minimum number of occurrences @{m,}@
  | Inv Int Int  -- ^ any number of occurrences within interval @{m,n}@
  deriving stock (Eq, Ord, Show, Read, Generic, Data)

instance Hashable ERE
instance Hashable Con
instance Hashable Exp
instance Hashable Dup

------------------------------------------------------------------------------

instance Pretty ERE where
  pretty = ann (Literal StringLit) . pretty . printERE

printERE :: ERE -> String
printERE (Alt xs) = concat $ List.intersperse "|" $ map printCon $ NE.toList xs
 where
  printCon (Con ys) = concatMap printExp $ NE.toList ys
  printExp = \case
    Chr c   -> [c]
    Per     -> "."
    Bra b   -> BE.printBE b
    Cir     -> "^"
    Dol     -> "$"
    Grp e   -> "(" ++ printERE e ++ ")"
    Dup e d -> printExp e ++ printDup d
  printDup = \case
    Ast     -> "*"
    Pls     -> "+"
    Que     -> "?"
    Exa m   -> "{" ++ show m ++ "}"
    Min m   -> "{" ++ show m ++ ",}"
    Inv m n -> "{" ++ show m ++ "," ++ show n ++ "}"

parseERE :: String -> Maybe ERE
parseERE = parseMaybe @Void ere

ere :: (MonadParsec e s m, Token s ~ Char) => m ERE
ere = Alt <$> NE.sepBy1 con (char '|')
 where
  con = Con <$> NE.some exp
  exp = do
    e <- choice [chr, per, bra, cir, dol, grp]
    d <- optional $ choice [ast, pls, que, try exa, try min, inv]
    return $ maybe e (Dup e) d
  chr = Chr <$> satisfy (`notElem` ("|.[^$()*?{" :: [Char]))
  per = Per <$ char '.'
  bra = Bra <$> BE.be
  cir = Cir <$ char '^'
  dol = Dol <$ char '$'
  grp = Grp <$ char '(' <*> ere <* char ')'
  ast = Ast <$ char '*'
  pls = Pls <$ char '+'
  que = Que <$ char '?'
  exa = Exa <$ char '{' <*> L.decimal                           <* char '}'
  min = Min <$ char '{' <*> L.decimal <* char ','               <* char '}'
  inv = Inv <$ char '{' <*> L.decimal <* char ',' <*> L.decimal <* char '}'

------------------------------------------------------------------------------

-- | Try to construct an 'ERE' from a 'Regex'. Returns 'Nothing' if the regex,
-- or one of its subexpressions, is empty (∅) or contains the empty word (ε), as
-- these cannot be represented in a POSIX ERE.
--
-- Note: According to the POSIX standard, a regular expression is not allowed to
-- contain @\NUL@. We deviate from the standard on this point, see the note at
-- 'BE.fromCharSet'. We consequently deviate in the meaning of the wildcard @.@
-- which we translate from Σ, i.e., the abstract character representing any
-- character from the full Unicode alphabet. Technically, @.@ should not match
-- @\NUL@ and thus would not be sufficient for representing Σ. But we don't want
-- to overcomplicate things---for now.
fromRegex :: Regex -> Maybe ERE
fromRegex = regexToAlt
 where
  regexToAlt :: Regex -> Maybe ERE
  regexToAlt = \case
    Plus xs  -> Alt <$> NE.fromList <$> mapM regexToCon xs
    r        -> Alt <$> NE.singleton <$> regexToCon r

  regexToCon :: Regex -> Maybe Con
  regexToCon = \case
    Times xs -> Con <$> NE.fromList <$> mapM regexToExp xs
    r        -> Con <$> NE.singleton <$> regexToExp r
  
  regexToExp :: Regex -> Maybe Exp
  regexToExp = \case
    One      -> Nothing
    Lit cs   -> litToExp cs
    Star r   -> regexToDup r Ast
    Opt r    -> regexToDup r Que
    r        -> Grp <$> regexToAlt r

  regexToDup :: Regex -> Dup -> Maybe Exp
  regexToDup r d = case r of
    Lit cs   -> Dup <$> litToExp cs <*> pure d
    Plus _   -> Dup <$> (Grp <$> regexToAlt r) <*> pure d
    Times _  -> Dup <$> (Grp <$> regexToAlt r) <*> pure d
    _        -> impossible

  litToExp cs
    | [c] <- CS.toList cs          = Just $ Chr c
    | CS.isFull cs                 = Just $ Per  -- note: deviation from standard
    | Just be <- BE.fromCharSet cs = Just $ Bra be
    | otherwise                    = Nothing


-- | Construct a 'Regex' from an 'ERE'.
--
-- Note: For consistency with 'fromRegex', the same deviations from the standard
-- as noted there apply (i.e., Σ is converted to @.@).
toRegex :: ERE -> Regex
toRegex ere0 = altToRegex ere0
 where
  altToRegex = \case
    Alt xs  -> Plus $ map conToRegex $ NE.toList xs
  
  conToRegex = \case
    Con xs  -> Times $ map expToRegex $ NE.toList xs
  
  expToRegex = \case
    Chr c   -> Lit $ CS.singleton c
    Per     -> AnyChar  -- note: deviation from standard    
    Bra b   -> Lit $ BE.toCharSet b
    Cir     -> panic $ "Cannot convert anchored ERE to Regex:" <+> pretty ere0
    Dol     -> panic $ "Cannot convert anchored ERE to Regex:" <+> pretty ere0
    Grp r   -> altToRegex r
    Dup r d -> dupToRegex (expToRegex r) d
  
  dupToRegex r = \case
    Ast     -> Star r
    Pls     -> r <> Star r
    Que     -> Opt r
    Exa m   -> Times (replicate m r)
    Min m   -> Times (replicate m r) <> Star r
    Inv m n -> Plus [Times (replicate i r) | i <- [m..n]]
