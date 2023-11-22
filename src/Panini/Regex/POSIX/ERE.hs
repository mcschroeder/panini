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

import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Panini.Regex.POSIX.BE (BE)
import Panini.Regex.POSIX.BE qualified as BE
import Panini.Regex.CharSet qualified as CS
import Prelude
import Panini.Regex.Type
import Panini.Panic

-- TODO: escaping
-- TODO: parsing
-- TODO: how to deal with ERE 'Per' excluding \NUL vs. Regex 'AnyChar' ?

------------------------------------------------------------------------------

-- | Extended Regular Expression (ERE) as defined by the POSIX standard.
data ERE
  = Alt (NonEmpty Con)  -- ^ alternation @x|y|z@

-- | ERE concatenation.
data Con
  = Con (NonEmpty Exp)  -- ^ concatenation @xyz@

-- | ERE (sub)expressions.
data Exp
  = Chr Char     -- ^ single ordinary character @a@
  | Per          -- ^ period @.@ (matches any character except @\NUL@)
  | Bra BE       -- ^ bracket expression @[abc]@, @[a-z]@, @[^abc]
  | Cir          -- ^ circumflex @^@ (matches beginning of string)
  | Dol          -- ^ dollar sign @$@ (matches end of string)
  | Grp ERE      -- ^ group @(x)@
  | Dup Exp Dup  -- ^ duplication @r*@, @r?@, @r{2,5}@

-- | ERE duplication symbols.
data Dup
  = Ast          -- ^ asterisk @*@ (matches zero or more expressions)
  | Pls          -- ^ plus sign @+@ (matches one or more expressions)
  | Que          -- ^ question mark @?@ (matches zero or one expressions)
  | Exa Int      -- ^ exact number of occurrences @{m}@
  | Min Int      -- ^ minimum number of occurrences @{m,}@
  | Inv Int Int  -- ^ any number of occurrences within interval @{m,n}@

------------------------------------------------------------------------------

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
    Word s   -> Con <$> NE.fromList <$> pure (map Chr s)
    Times xs -> Con <$> NE.fromList <$> mapM regexToExp xs
    r        -> Con <$> NE.singleton <$> regexToExp r
  
  regexToExp :: Regex -> Maybe Exp
  regexToExp = \case
    One      -> Nothing
    Lit cs   -> litToExp cs
    Word [c] -> pure $ Chr c
    Star r   -> regexToDup r Ast
    Opt r    -> regexToDup r Que
    r        -> Grp <$> regexToAlt r

  regexToDup :: Regex -> Dup -> Maybe Exp
  regexToDup r d = case r of
    Lit cs   -> Dup <$> litToExp cs <*> pure d
    Word [c] -> Dup <$> pure (Chr c) <*> pure d
    Word _   -> Dup <$> (Grp <$> regexToAlt r) <*> pure d
    Plus _   -> Dup <$> (Grp <$> regexToAlt r) <*> pure d
    Times _  -> Dup <$> (Grp <$> regexToAlt r) <*> pure d
    _        -> impossible

  litToExp cs
    | [c] <- CS.toList cs          = Just $ Chr c
    | CS.isFull cs                 = Just $ Per  -- note: deviation from standard
    | Just be <- BE.fromCharSet cs = Just $ Bra be
    | otherwise                    = Nothing
