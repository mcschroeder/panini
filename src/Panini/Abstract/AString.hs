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
import Data.GSet -- from regexp
import Data.Hashable
import Data.Set qualified as S
import GHC.Generics (Generic)
import Panini.Abstract.AChar (AChar)
import Panini.Abstract.AChar qualified as AChar
import Panini.Pretty
import Panini.SMT.RegLan qualified as SMT
import Prelude
import Panini.Regex
import Data.String

-- TODO: add conversion to/from POSIX patterns (BRE)

------------------------------------------------------------------------------

newtype AString = AString Regex
  deriving stock (Eq, Show, Read, Generic)  
  deriving newtype 
    ( Semigroup, Monoid
    , JoinSemilattice, BoundedJoinSemilattice
    , MeetSemilattice, BoundedMeetSemilattice
    , ComplementedLattice
    )

instance Hashable AString where
  hashWithSalt s (AString r) = 1 -- TODO

eq :: String -> AString
eq = AString . fromString

lit :: AChar -> AString
lit = AString . Lit

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
  Lit c    -> Just c
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
      Lit c -> pretty c
      Word [] -> epsilon
      Word s -> pretty s
      Plus rs -> parensIf o $ concatWithOp "|" $ map (go False) rs
      Times rs -> mconcat $ map (go True) rs
      Star r@(Lit _) -> pretty r <> "*"
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
    Lit c -> case AChar.toFiniteSet c of
      These a -> charsetToRegLan a
      ComplementOf a -> SMT.Diff SMT.AllChar (charsetToRegLan a)
    Word s -> SMT.ToRe s
    Plus rs -> foldr1 SMT.Union (map go rs)
    Times rs -> foldr1 SMT.Conc (map go rs)
    Star r -> SMT.Star (go r)
    Opt r -> SMT.Opt (go r)
    
  charsetToRegLan cs = case (S.size cs, S.findMin cs, S.findMax cs) of
      (1,l,_) -> SMT.ToRe [l]
      (n,l,u) | fromEnum u - fromEnum l + 1 == n -> SMT.Range l u
      _ -> foldr1 SMT.Union $ map (SMT.ToRe . pure) $ S.toList cs
