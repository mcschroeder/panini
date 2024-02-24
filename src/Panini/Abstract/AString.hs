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
  , toRegex
  , fromRegex
  , toRegLan
  , length
  , Panini.Abstract.AString.simplify  
  ) where

import Algebra.Lattice
import Data.Hashable
import Data.String
import GHC.Generics (Generic)
import Panini.Abstract.AChar (AChar)
import Panini.Abstract.AChar qualified as AChar
import Panini.Abstract.AInt (AInt)
import Panini.Abstract.AInt qualified as AInt
import Panini.Pretty
import Panini.Regex
import Panini.Regex.Type (minWordLength, maxWordLength)
import Panini.Regex.SMT qualified
import Panini.SMT.RegLan (RegLan)
import Prelude hiding (length)

------------------------------------------------------------------------------

newtype AString = AString Regex
  deriving stock (Eq, Show, Read, Generic)
  deriving newtype (Semigroup, Monoid, Hashable)

instance JoinSemilattice AString where
  AString r1 ∨ AString r2 = AString (Plus [r1,r2])

instance BoundedJoinSemilattice AString where
  bot = AString Zero

instance MeetSemilattice AString where
  AString r1 ∧ AString r2 = AString $ intersection r1 r2

instance BoundedMeetSemilattice AString where
  top = AString All

instance ComplementedLattice AString where
  neg (AString r) = AString $ complement r

instance Pretty AString where
  pretty (AString r) = ann (Literal AbstractLit) $ pretty r

------------------------------------------------------------------------------

simplify :: AString -> AString
simplify (AString r) = AString $ Panini.Regex.simplify r

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

------------------------------------------------------------------------------

toChar :: AString -> Maybe AChar
toChar (AString r) = case Panini.Regex.simplify r of
  Lit c    -> Just (AChar.fromCharSet c)
  _        -> Nothing

toRegex :: AString -> Regex
toRegex (AString r) = r

fromRegex :: Regex -> AString
fromRegex = AString

-- TODO: replace with SMTLIB instance
toRegLan :: AString -> RegLan
toRegLan (AString r) = Panini.Regex.SMT.toRegLan r

------------------------------------------------------------------------------

length :: AString -> AInt
length (AString r) = case (minWordLength r, maxWordLength r) of
  (Just m, Just (-1)) -> AInt.ge (fromIntegral m)
  (Just m, Just n   ) -> AInt.ge (fromIntegral m) ∧ AInt.le (fromIntegral n)
  _                   -> bot
