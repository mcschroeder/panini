{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Panini.Abstract.AString where

import Algebra.Lattice
import Data.Hashable
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Panini.Abstract.AChar (AChar)
import Panini.Abstract.AChar qualified as AChar
import Panini.Abstract.AInt (AInt)
import Panini.Abstract.AInt qualified as AInt
import Panini.Pretty
import Panini.Regex.Operations qualified as Regex
import Panini.Regex.Simplify qualified as Regex
import Panini.Regex.SMT qualified as Regex
import Panini.Regex.Type
import Panini.SMT.RegLan (RegLan)
import Prelude hiding (length)

------------------------------------------------------------------------------

newtype AString = MkAString Regex
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving newtype (Semigroup, Monoid, Hashable)

-- | Based on total structural ordering of 'Regex'.
instance PartialOrder AString where
  (⊑) = (<=)

instance JoinSemilattice AString where
  MkAString r1 ∨ MkAString r2 = simplify $ MkAString $ Plus [r1,r2]

instance BoundedJoinSemilattice AString where
  bot = MkAString Zero

instance MeetSemilattice AString where
  MkAString r1 ∧ MkAString r2 = simplify $ MkAString $ Regex.intersection r1 r2

instance BoundedMeetSemilattice AString where
  top = MkAString All

instance ComplementedLattice AString where
  neg (MkAString r) = simplify $ MkAString $ Regex.complement r

instance Pretty AString where
  pretty (MkAString r) = ann (Literal AbstractLit) $ pretty r

------------------------------------------------------------------------------

simplify :: AString -> AString
simplify (MkAString r) = MkAString $ Regex.simplify r

eq :: String -> AString
eq = MkAString . fromString

lit :: AChar -> AString
lit = MkAString . Lit . AChar.toCharSet

rep :: AString -> Integer -> AString
rep a n
  | n < 0     = bot
  | otherwise = mconcat $ replicate (fromIntegral n) a

rep2 :: AString -> Integer -> Integer -> AString
rep2 a m n
  | 0 <= m, m <= n = rep a m <> (joins $ map (rep a) [0 .. m - n])
  | otherwise      = bot

anyChar :: AString
anyChar = MkAString AnyChar

star :: AString -> AString
star (MkAString r) = MkAString (Star r)

-- | r?
opt :: AString -> AString
opt (MkAString r) = MkAString (Opt r)

------------------------------------------------------------------------------

member :: String -> AString -> Bool
member s (MkAString r) = Regex.membership s r

toChar :: AString -> Maybe AChar
toChar (MkAString r) = case Regex.simplify r of
  Lit c    -> Just (AChar.fromCharSet c)
  _        -> Nothing

toRegex :: AString -> Regex
toRegex (MkAString r) = r

fromRegex :: Regex -> AString
fromRegex = MkAString

-- TODO: replace with SMTLIB instance
toRegLan :: AString -> RegLan
toRegLan (MkAString r) = Regex.toRegLan r

------------------------------------------------------------------------------

-- | Returns the lower and upper bounds on the abstract string's length. The
-- length of any word represented by the string falls within this range. Note,
-- however, that not every length within this range is necessarily represented
-- by the string, e.g., the length bounds of "(ab)*" are [0,+∞], but the string
-- does not contain any words of length 1 or 3 or 5, etc.
lengthBounds :: AString -> AInt
lengthBounds (MkAString r) = case (minWordLength r, maxWordLength r) of
  (Just m, Just (-1)) -> AInt.ge (fromIntegral m)
  (Just m, Just n   ) -> AInt.ge (fromIntegral m) ∧ AInt.le (fromIntegral n)
  _                   -> bot

equiv :: AString -> AString -> Bool
equiv (MkAString a) (MkAString b) = Regex.equivalence a b

isEmpty :: AString -> Bool
isEmpty (MkAString One) = True
isEmpty _               = False

------------------------------------------------------------------------------

-- | Returns the length of an abstract string that represents only strings of a
-- single length, or 'Nothing' if the abstract string represents strings of
-- multiple different lengths.
-- ⟦ |ŝ| = n ⟧↑n = |ŝ| if ∀{s₁∈ŝ,s₂∈ŝ} |s₁| = |s₂|
strLen1 :: AString -> Maybe Integer
strLen1 a = case AInt.values (lengthBounds a) of
  [n] -> Just n
  _   -> Nothing

-- ⟦ s[î] = c ⟧↑c ≐ ⋁{i∈î} s[i]
charAt :: Text -> AInt -> AChar
charAt s a = 
  joins [ AChar.eq $ Text.index s i 
        | i <- map fromIntegral $ AInt.values $ a ∧ safeBounds
        ]
 where
  safeBounds = AInt.ge 0 ∧ AInt.lt (fromIntegral $ Text.length s)

-- ⟦ s[î..ĵ] = t ⟧↑t ≐ ⋁{i∈î and j∈ĵ} s[i..j]
strSub :: Text -> AInt -> AInt -> AString
strSub s a b = 
  joins [ eq $ Text.unpack $ Text.take (j - i + i) $ Text.drop i s
        | i <- map fromIntegral $ AInt.values $ (a ∧ safeBounds)
        , j <- map fromIntegral $ AInt.values $ (b ∧ safeBounds)
        ]
 where
  safeBounds = AInt.ge 0 ∧ AInt.lt (fromIntegral $ Text.length s)
