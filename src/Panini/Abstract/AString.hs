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
  , toChar
  , toRegLan
  ) where

import Algebra.Lattice
import Data.Hashable
import Data.IntSet qualified as IS
import Data.String
import GHC.Generics (Generic)
import Panini.Abstract.AChar (AChar)
import Panini.Abstract.AChar qualified as AChar
import Panini.Pretty
import Panini.Regex
import Panini.Regex.CharSet qualified as CS
import Panini.SMT.RegLan qualified as SMT
import Prelude

-- TODO: add conversion to/from POSIX patterns (BRE)

------------------------------------------------------------------------------

newtype AString = AString Regex
  deriving stock (Eq, Show, Read, Generic)  
  deriving newtype 
    ( Semigroup, Monoid
    , JoinSemilattice, BoundedJoinSemilattice
    , MeetSemilattice, BoundedMeetSemilattice
    , ComplementedLattice
    , Hashable
    )

eq :: String -> AString
eq = AString . fromString

lit :: AChar -> AString
lit = AString . Lit . AChar.toCharSet

rep :: AString -> Integer -> AString
rep a n = mconcat $ replicate (fromIntegral n) a

anyChar :: AString
anyChar = AString AnyChar

star :: AString -> AString
star (AString r) = AString (Star r)

-- | r?
opt :: AString -> AString
opt (AString r) = AString (Opt r)

toChar :: AString -> Maybe AChar
toChar (AString r) = case simplify r of
  Lit c    -> Just (AChar.fromCharSet c)
  Word [c] -> Just (AChar.eq c)
  _        -> Nothing

-- TODO: actual regex syntax (.) vs formalized view (Σ)
-- TODO: unify pretty printing Regex vs AString
instance Pretty AString where
  pretty (AString r0) = go True r0
   where
    go o = \case
      Zero -> emptySet
      One -> epsilon
      AnyChar -> bigSigma
      All -> bigSigma <> "*"
      Lit c -> pretty (AChar.fromCharSet c)
      Word [] -> epsilon
      Word s -> pretty s
      Plus rs -> parensIf o $ concatWithOp "|" $ map (go False) rs
      Times rs -> mconcat $ map (go True) rs
      Star r@(Lit _) -> pretty (AString r) <> "*"
      Star r -> parens (go False r) <> "*"
      Opt r -> parens (go False r) <> "?"

------------------------------------------------------------------------------

toRegLan :: AString -> SMT.RegLan
toRegLan (AString r0) = go r0
 where
  go = \case
    Zero -> SMT.None
    One -> SMT.ToRe ""    
    AnyChar -> SMT.AllChar
    All -> SMT.All
    Lit c -> case CS.fromCharSet c of
      (True, a) -> charsetToRegLan a
      (False, a) -> SMT.Diff SMT.AllChar (charsetToRegLan a)
    Word s -> SMT.ToRe s
    Plus rs -> foldr1 SMT.Union (map go rs)
    Times rs -> foldr1 SMT.Conc (map go rs)
    Star r -> SMT.Star (go r)
    Opt r -> SMT.Opt (go r)
    
  charsetToRegLan cs = case (IS.size cs, IS.findMin cs, IS.findMax cs) of
      (1,l,_) -> SMT.ToRe [toEnum @Char l]
      (n,l,u) | fromEnum u - fromEnum l + 1 == n -> SMT.Range (toEnum @Char l) (toEnum @Char u)
      _ -> foldr1 SMT.Union $ map (SMT.ToRe . pure . toEnum @Char) $ IS.toList cs
