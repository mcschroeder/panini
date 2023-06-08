{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Panini.Solver.Abstract.AString
  ( AString
  , aStringLit
  , aStringRep
  , aStringSigma
  , aStringStar
  ) where

import Prelude
import Panini.Solver.Abstract.AChar
import Panini.Algebra.Lattice
import Panini.Pretty.Printer hiding (Literal)
import GHC.Generics (Generic)
import Data.Hashable

import RegExp.RegExp
import RegExp.Operations

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
  (⊤) = aStringStar aStringSigma

instance JoinSemilattice AString where
  AString r1 ∨ AString r2 = AString $ rPlus r1 r2
  
instance BoundedJoinSemilattice AString where
  (⊥) = AString rZero

aStringLit :: AChar -> AString
aStringLit = AString . rLiteral . aCharToFiniteSet

aStringRep :: AString -> Integer -> AString
aStringRep a n = mconcat $ replicate (fromIntegral n) a

aStringSigma :: AString
aStringSigma = AString $ rLiteral $ aCharToFiniteSet (⊤)

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



