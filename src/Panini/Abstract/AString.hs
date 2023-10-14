{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Panini.Abstract.AString
  ( AString
  , eq
  , lit
  , rep
  , anyChar
  , star
  , opt
  , toRegLan
  ) where

import Algebra.Lattice
import Data.GSet -- from regexp
import Data.Hashable
import Data.Set qualified as S
import GHC.Generics (Generic)
import Panini.Abstract.AChar (AChar)
import Panini.Abstract.AChar qualified as AChar
import Panini.Pretty hiding (Literal)
import Panini.SMT.RegLan qualified as SMT
import Prelude
import RegExp.Operations
import RegExp.RegExp

-- TODO: add sophisticated regex simplifier (cf. Kahrs/Runciman 2022)
-- TODO: add conversion to/from POSIX patterns (BRE)

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
  top = star anyChar

instance JoinSemilattice AString where
  AString r1 ∨ AString r2 = AString $ rPlus r1 r2
  
instance BoundedJoinSemilattice AString where
  bot = AString rZero

eq :: String -> AString
eq = mconcat . map (lit . AChar.eq)

lit :: AChar -> AString
lit = AString . rLiteral . AChar.toFiniteSet

rep :: AString -> Integer -> AString
rep a n = mconcat $ replicate (fromIntegral n) a

anyChar :: AString
anyChar = AString $ rLiteral $ AChar.toFiniteSet top

star :: AString -> AString
star (AString r) = AString $ rStar r

-- | r?
opt :: AString -> AString
opt (AString r) = AString (rOne `rPlus` r)

instance Pretty AString where
  pretty (AString r) = pretty $ view r

-- TODO: actual regex syntax (.) vs formalized view (Σ)
instance Pretty (RegExpView Char (RegExp Char)) where
  pretty = pretty' True
   where
    pretty' open = \case
      One -> epsilon
      Plus (view -> a) (view -> b) -> 
        parensIf open $ pretty' False a <+> "|" <+> pretty' False b
      Times (view -> a) (view -> b) -> pretty a <> pretty b
      Star (view -> a) -> case a of
        Literal c -> pretty (AChar.fromFiniteSet c) <> "*"
        _ -> parens (pretty' False a) <> "*"
      Literal c -> pretty (AChar.fromFiniteSet c)

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
