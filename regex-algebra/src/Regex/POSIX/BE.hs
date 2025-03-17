{-| 
This module implements types and functions for handling POSIX Bracket
Expressions (BEs) which occur inside Extended Regular Expressions (EREs).

References:

  * Goyvaerts, Jan. 2023. "POSIX Bracket Expressions." Last updated August 29,
    2023. https://www.regular-expressions.info/posixbrackets.html

  * IEEE and The Open Group. 2018. "Regular Expressions." Chap. 9 in The Open
    Group Base Specifications Issue 7 (IEEE Std 1003.1-2017).
    https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap09.html

  * Wikipedia. 2023. "Regular Expression." Accessed November 22, 2023.
    https://en.wikipedia.org/wiki/Regular_expression

-}
module Regex.POSIX.BE where

import Control.Monad.Combinators.NonEmpty qualified as NE
import Data.Char (showLitChar)
import Data.Data (Data)
import Data.Hashable
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Void
import GHC.Generics
import Prelude hiding (exp)
import Regex.CharSet (CharSet)
import Regex.CharSet qualified as CS
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Proxy

-- TODO: escaping
-- TODO: support equivalence classes
-- TODO: support collating symbols
-- TODO: more efficient conversion from/to CharSet (recognize ranges)
-- TODO: support different locales for character classes

-------------------------------------------------------------------------------

-- | Bracket Expression (BE) as defined by the POSIX standard.
data BE
  = Mat (NonEmpty Exp)  -- ^ matching expression @[abc]@
  | Non (NonEmpty Exp)  -- ^ non-matching expression @[^abc]@
  deriving stock (Eq, Ord, Show, Read, Generic, Data)

-- | BE (sub)expression.
data Exp
  = Ord Char       -- ^ ordinary character @a@
  | Ran Char Char  -- ^ range of consecutive characters @a-z@
  | Cls CharClass  -- ^ character class @[:alpha:]@ (ASCII only)
  -- | Equ         -- ^ equivalence class @[=a=]@
  -- | Col         -- ^ collating symbol @[.ch.]@
  deriving stock (Eq, Ord, Show, Read, Generic, Data)

data CharClass
  = Upper -- ^ uppercase letters @[A-Z]@
  | Lower -- ^ lowercase letters @[a-z]@
  | Alpha -- ^ upper- and lowercase letters @[[:upper:][:lower:]]@
  | Alnum -- ^ digits, upper- and lowercase letters @[[:alpha:][:digit:]]@
  | Digit -- ^ digits @[0-9]@
  | Xdigit -- ^ hexadecimal digits @[0-9A-Fa-f]@
  | Punct -- ^ punctuation @[.,!?:…]@
  | Blank -- ^ space and TAB characters only @[ \t]@
  | Space -- ^ blank (whitespace) characters @[ \t\n\r\f\v]@
  | Cntrl -- ^ control characters
  | Graph -- ^ printed characters @[^\t\n\r\f\v]@
  | Print -- ^ printed characters and space @[^ \t\n\r\f\v]@
  deriving stock (Eq, Ord, Show, Read, Generic, Data)

instance Hashable BE
instance Hashable Exp
instance Hashable CharClass

-------------------------------------------------------------------------------

printBE :: BE -> String
printBE = \case
  Mat xs -> "["  <> mconcat (map go $ NE.toList xs) <> "]"
  Non xs -> "[^" <> mconcat (map go $ NE.toList xs) <> "]"
 where  
  go = \case
    Ord c     -> showLitChar c ""  -- TODO: ensure this escaping matches POSIX
    Ran c1 c2 -> foldr showLitChar "" [c1,'-',c2]
    Cls Upper -> "[:upper:]"
    Cls Lower -> "[:lower:]"
    Cls Alpha -> "[:alpha:]"
    Cls Alnum -> "[:alnum:]"
    Cls Digit -> "[:digit:]"
    Cls Xdigit -> "[:xdigit:]"
    Cls Punct -> "[:punct:]"
    Cls Blank -> "[:blank:]"
    Cls Space -> "[:space:]"
    Cls Cntrl -> "[:cntrl:]"
    Cls Graph -> "[:graph:]"
    Cls Print -> "[:print:]"

parseBE :: String -> Maybe BE
parseBE = parseMaybe @Void be

be :: (MonadParsec e s m, Token s ~ Char) => m BE
be = char '[' *> choice [non, mat] <* char ']'
 where
  non = Non <$ char '^' <*> NE.some exp
  mat = Mat <$>             NE.some exp
  exp = choice [try ran, try cls, ord]
  ran = Ran <$> chr <* char '-' <*> chr
  cls = Cls <$> charClass
  ord = Ord <$> chr
  chr = satisfy (\x -> x /= ']')

charClass :: forall e s m. (MonadParsec e s m, Token s ~ Char) => m CharClass
charClass = choice 
  [ Upper <$ str "[:upper:]"
  , Lower <$ str "[:lower:]"
  , Alpha <$ str "[:alpha:]"
  , Alnum <$ str "[:alnum:]"
  , Digit <$ str "[:digit:]"
  , Xdigit <$ str "[:xdigit:]"
  , Punct <$ str "[:punct:]"
  , Blank <$ str "[:blank:]"
  , Space <$ str "[:space:]"
  , Cntrl <$ str "[:cntrl:]"
  , Graph <$ str "[:graph:]"
  , Print <$ str "[:print:]"
  ]
 where
  str s = string (tokensToChunk (Proxy :: Proxy s) s)

-------------------------------------------------------------------------------

-- | Try to construct a 'BE' from a 'CharSet'. Returns 'Nothing' if the
-- character set is empty (∅), since BEs cannot represent empty sets.
--
-- Note: According to the POSIX standard, a regular expression is not allowed to
-- contain @\NUL@. We deviate from the standard on this point and will create a
-- BE containing @\NUL@ if @\NUL@ is in the 'CharSet'. In particular, the full
-- character set (Σ), representing the entire Unicode alphabet, will be
-- converted to the BE @[\NUL-\u1114111]@ where @\u1114111@ is the 'maxBound' of
-- 'Char'. This is in line with how Σ is usually treated, e.g., by translating
-- it to the ERE wildcard @.@ which also technically should exclude @\NUL@.
fromCharSet :: CharSet -> Maybe BE
fromCharSet (CS.fromCharSet -> (b,s)) = case (b, NE.nonEmpty $ CS.intSetToCharList s) of
  (True,  Nothing) -> Nothing
  (True,  Just xs) -> Just $ Mat $ NE.fromList $ rangeChunks $ NE.toList xs
  (False, Nothing) -> Just $ Mat $ NE.singleton $ Ran '\NUL' (maxBound @Char)
  (False, Just xs) -> Just $ Non $ NE.map Ord xs

rangeChunks :: [Char] -> [Exp]
rangeChunks = foldr go []
 where  
  go x (Ord y : Ord z : cs) | y `succeeds` x, z `succeeds` y = Ran x z : cs
  go x (Ran y z       : cs) | y `succeeds` x                 = Ran x z : cs
  go x                  cs                                   = Ord x : cs
  
  succeeds y x = x /= maxBound && y == succ x

-- | Construct a 'CharSet' from a 'BE'.
toCharSet :: BE -> CharSet
toCharSet = \case
  Mat xs -> CS.fromList $ concatMap go xs
  Non xs -> CS.complement $ CS.fromList $ concatMap go xs
 where
  go = \case
    Ord c     -> [c]
    Ran c1 c2 -> enumFromTo c1 c2
    Cls Upper -> ['A'..'Z']
    Cls Lower -> ['a'..'z']
    Cls Alpha -> ['A'..'Z'] <> ['a'..'z']
    Cls Alnum -> ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9']
    Cls Digit -> ['0'..'9']
    Cls Xdigit -> ['0'..'9'] <> ['A'..'F'] <> ['a'..'f']
    Cls Punct -> ".,!?:…"
    Cls Blank -> " \t"
    Cls Space -> " \t\n\r\f\v"
    Cls Cntrl -> ['\x00'..'\x1F'] <> "\x7F"
    Cls Graph -> ['\x21'..'\x7E']
    Cls Print -> ['\x20'..'\x7E']
  