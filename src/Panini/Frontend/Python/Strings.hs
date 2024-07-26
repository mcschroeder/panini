module Panini.Frontend.Python.Strings where

import Data.Char
import Data.List.Extra
import Prelude

-- | Decode a Python string literal with enclosing quotes and optional prefix
-- characters; returns 'Nothing' if the literal is malformed or unsupported.
--
-- The following types of Python string literals are supported:
--
--  * strings enclosed in matching single (') or double (") quotes
--  * triple-quoted strings (enclosed in matching ''' or """ quotes)
--  * raw strings (prefixed with 'r' or 'R')
--
-- Not supported are f-strings, Python 2 Unicode strings, or Bytes literals.
--
-- Reference:
--   https://docs.python.org/3/reference/lexical_analysis.html#string-and-bytes-literals
decodeStringLiteral :: String -> Maybe String
decodeStringLiteral = \case
  'r':cs -> unquote cs
  'R':cs -> unquote cs
  cs     -> unescape <$> unquote cs
 where
  unquote = \case
    '\"':'\"':'\"':cs -> Just $ dropEnd 3 cs
    '\'':'\'':'\'':cs -> Just $ dropEnd 3 cs
    '\"'          :cs -> Just $ init cs
    '\''          :cs -> Just $ init cs
    _                 -> Nothing

-- | Interpret all escape sequences in a Python string, i.e., "unescaping" it.
--
-- Note: @\\N{name}@ escapes are not supported and left in the string unchanged.
--
-- Reference:
--   https://docs.python.org/3/reference/lexical_analysis.html#escape-sequences
unescape :: String -> String
unescape = \case
  '\\':'\n':cs ->        unescape cs  -- Backslash and newline ignored
  '\\':'\\':cs -> '\\' : unescape cs  -- Backslash (\)
  '\\':'\'':cs -> '\'' : unescape cs  -- Single quote (')
  '\\':'\"':cs -> '\"' : unescape cs  -- Double quote (")
  '\\':'a' :cs -> '\a' : unescape cs  -- ASCII Bell (BEL)
  '\\':'b' :cs -> '\b' : unescape cs  -- ASCII Backspace (BS)
  '\\':'f' :cs -> '\f' : unescape cs  -- ASCII Formfeed (FF)
  '\\':'n' :cs -> '\n' : unescape cs  -- ASCII Linefeed (LF)
  '\\':'r' :cs -> '\r' : unescape cs  -- ASCII Carriage Return (CR)
  '\\':'t' :cs -> '\t' : unescape cs  -- ASCII Horizontal Tab (TAB)
  '\\':'v' :cs -> '\v' : unescape cs  -- ASCII Vertical Tab (VT)    
  
  -- Character with octal value ooo
  '\\':o1:o2:o3:cs | Just c <- readOctDigits [o1,o2,o3] -> c : unescape cs

  -- Character with hex value hh
  '\\':'x':h1:h2:cs | Just c <- readHexDigits [h1,h2] -> c : unescape cs

  -- Character with 16-bit hex value xxxx
  '\\':'u':x1:x2:x3:x4:cs 
    | Just c <- readHexDigits [x1,x2,x3,x4] -> c : unescape cs
  
  -- Character with 32-bit hex value xxxxxxxx
  '\\':'U':x1:x2:x3:x4:x5:x6:x7:x8:cs
    | Just c <- readHexDigits [x1,x2,x3,x4,x5,x6,x7,x8] -> c : unescape cs

  c:cs -> c : unescape cs
  []   -> []

readOctDigits :: [Char] -> Maybe Char
readOctDigits cs = case readLitChar ("\\o" ++ cs) of
  [(c,[])] -> Just c
  _        -> Nothing

readHexDigits :: [Char] -> Maybe Char
readHexDigits cs = case readLitChar ("\\x" ++ cs) of
  [(c,[])] -> Just c
  _        -> Nothing