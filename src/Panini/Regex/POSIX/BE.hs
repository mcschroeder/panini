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
module Panini.Regex.POSIX.BE where

import Data.Data (Data)
import Data.Char
import Data.Hashable
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import GHC.Generics
import Panini.Pretty
import Panini.Regex.CharSet (CharSet(..))
import Panini.Regex.CharSet qualified as CS
import Prelude

-- TODO: escaping
-- TODO: support character classes
-- TODO: support equivalence classes
-- TODO: support collating symbols
-- TODO: parsing
-- TODO: more efficient conversion from/to CharSet (recognize ranges)

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
  -- | Cls         -- ^ character class @[:alpha:]@
  -- | Equ         -- ^ equivalence class @[=a=]@
  -- | Col         -- ^ collating symbol @[.ch.]@
  deriving stock (Eq, Ord, Show, Read, Generic, Data)

instance Hashable BE
instance Hashable Exp

-------------------------------------------------------------------------------

instance Pretty BE where
  pretty = ann (Literal StringLit) . pretty . printBE

printBE :: BE -> String
printBE = \case
  Mat xs -> "["  <> mconcat (map go $ NE.toList xs) <> "]"
  Non xs -> "[^" <> mconcat (map go $ NE.toList xs) <> "]"
 where  
  go = \case
    Ord c     -> [c]
    Ran c1 c2 -> c1:'-':c2:[]

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
fromCharSet (CharSet b s) = case (b, NE.nonEmpty $ CS.intSetToCharList s) of
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
