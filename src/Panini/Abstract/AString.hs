{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Panini.Abstract.AString
  ( AString
  , aStringLit
  , aStringRep
  , aStringSigma
  , aStringStar
  , RE(..)
  , toRegex
  ) where

import Data.Hashable
import GHC.Generics (Generic)
import Panini.Abstract.AChar
import Panini.Algebra.Lattice
import Panini.Pretty.Printer hiding (Literal)
import Prelude
import RegExp.Operations
import RegExp.RegExp
import Data.Set qualified as S
import Data.GSet -- from regexp

------------------------------------------------------------------------------

data AString = AString (RegExp Char)
  deriving stock (Eq, Show, Read, Generic)

instance Hashable AString where
  hashWithSalt s (AString r) = hashUsing view s r

instance Semigroup AString where
  AString r1 <> AString r2 = AString $ rTimes r1 r2
  
instance Monoid AString where
  mempty = AString rOne

instance MeetSemilattice AString where
  AString r1 ∧ AString r2 = AString $ intersection r1 r2

instance BoundedMeetSemilattice AString where
  top = aStringStar aStringSigma

instance JoinSemilattice AString where
  AString r1 ∨ AString r2 = AString $ rPlus r1 r2
  
instance BoundedJoinSemilattice AString where
  bot = AString rZero

aStringLit :: AChar -> AString
aStringLit = AString . rLiteral . aCharToFiniteSet

aStringRep :: AString -> Integer -> AString
aStringRep a n = mconcat $ replicate (fromIntegral n) a

aStringSigma :: AString
aStringSigma = AString $ rLiteral $ aCharToFiniteSet top

aStringStar :: AString -> AString
aStringStar (AString r) = AString $ rStar r

instance Pretty AString where
  pretty (AString r) = pretty $ view r

instance Pretty (RegExpView Char (RegExp Char)) where
  pretty = pretty' True
   where
    pretty' open = \case
      One -> "ε"
      Plus (view -> a) (view -> b) -> 
        parensIf open $ pretty' False a <+> "|" <+> pretty' False b
      Times (view -> a) (view -> b) -> pretty a <> pretty b
      Star (view -> a) -> case a of
        Literal c -> pretty (finiteSetToAChar c) <> "*"
        _ -> parens (pretty' False a) <> "*"
      Literal c -> pretty (finiteSetToAChar c)

------------------------------------------------------------------------------

-- TODO: improve regex representation

data RE = Empty | Concat RE RE | Union RE RE | KleeneStar RE | Lit String | Diff RE RE | AllChars

toRegex :: AString -> RE
toRegex (AString s) = go $ view s
  where
    go = \case
      One -> Empty
      Plus a b -> Union (go $ view a) (go $ view b)
      Times a b -> Concat (go $ view a) (go $ view b)  -- TODO: detect strings
      Star a -> KleeneStar (go $ view a)
      Literal c -> case c of
        These a -> if null a then Empty else charsetToRE a
        ComplementOf a -> if null a then AllChars else Diff AllChars (charsetToRE a)
    
    -- TODO: detect ranges
    charsetToRE = foldr1 Union . map (Lit . pure) . S.toList

