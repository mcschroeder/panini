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

import Data.String
import GHC.Generics (Generic)
import Panini.Abstract.AChar (AChar)
import Panini.Abstract.AChar qualified as AChar
import Panini.Pretty
import Panini.Regex
import Panini.Regex.POSIX
import Panini.Regex.SMT qualified
import Panini.SMT.RegLan (RegLan)
import Prelude

-- TODO: add conversion to/from POSIX patterns (BRE)
-- TODO: AString pretty printing vs. conversion to ERE

------------------------------------------------------------------------------

newtype AString = AString Regex
  deriving stock (Eq, Show, Read, Generic)
  deriving newtype (Semigroup, Monoid, Hashable)

instance JoinSemilattice AString where
  AString r1 ∨ AString r2 = AString (Plus [r1,r2])

instance BoundedJoinSemilattice AString where
  bot = AString Zero

instance MeetSemilattice AString where
  AString r1 ∧ AString r2 = AString $ simplify $ intersection r1 r2

instance BoundedMeetSemilattice AString where
  top = AString All

instance ComplementedLattice AString where
  neg (AString r) = AString $ simplify $ complement r

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

-- TODO: separate pretty printing / ERE printing?
instance Pretty AString where
  pretty (AString r) = pretty $ printERE $ toERE r

-- TODO: replace with SMTLIB instance
toRegLan :: AString -> RegLan
toRegLan (AString r) = Panini.Regex.SMT.toRegLan r
