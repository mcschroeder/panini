{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Panini.Abstract.AString
  ( AString
  , aStringLit
  , aStringRep
  , aStringSigma
  , aStringStar
  , toRegLan
  ) where

import Algebra.Lattice
import Data.GSet -- from regexp
import Data.Hashable
import Data.Set qualified as S
import GHC.Generics (Generic)
import Panini.Abstract.AChar
import Panini.Pretty.Printer hiding (Literal)
import Panini.SMT.RegLan qualified as SMT
import Prelude
import RegExp.Operations
import RegExp.RegExp

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

toRegLan :: AString -> SMT.RegLan
toRegLan (AString s) = go $ view s
  where
    go = \case
      One -> SMT.None
      Plus a b -> SMT.Union (go $ view a) (go $ view b)
      Times a b -> SMT.Conc (go $ view a) (go $ view b)
      Star (view -> Literal (ComplementOf a)) | null a -> SMT.All
      Star a -> SMT.Star (go $ view a)
      Literal (These a)
        | null a -> SMT.None
        | otherwise -> charsetToRegLan a
      Literal (ComplementOf a) 
        | null a -> SMT.AllChar
        | otherwise -> SMT.Diff SMT.AllChar (charsetToRegLan a)
    
    charsetToRegLan cs = case (S.size cs, S.findMin cs, S.findMax cs) of
      (1,l,_) -> SMT.ToRe [l]
      (n,l,u) | fromEnum u - fromEnum l + 1 == n -> SMT.Range l u
      _ -> foldr1 SMT.Union $ map (SMT.ToRe . pure) $ S.toList cs
