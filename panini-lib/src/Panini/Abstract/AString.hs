module Panini.Abstract.AString where

import Algebra.Lattice
import Data.Data (Data)
import Data.Hashable
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Panini.Abstract.AChar (AChar)
import Panini.Abstract.AChar qualified as AChar
import Panini.Abstract.AInt (AInt)
import Panini.Abstract.AInt qualified as AInt
import Panini.Abstract.Interval (Inf(..), pattern (:…))
import Panini.Panic
import Panini.Pretty
import Panini.Regex.SMT qualified as Regex
import Panini.SMT.RegLan (RegLan)
import Prelude hiding (length)
import Regex qualified as Regex
import Regex.CharSet qualified as CS
import Regex.Inclusion qualified as Regex
import Regex.POSIX.ERE qualified
import Regex.Type

------------------------------------------------------------------------------

newtype AString = MkAString Regex
  deriving stock (Eq, Ord, Show, Read, Generic, Data)
  deriving newtype (Semigroup, Monoid, Hashable)

-- | Regular language inclusion. CAUTION: this is an expensive operation!
instance PartialOrder AString where
  (⊑) = isIncludedBy

instance JoinSemilattice AString where
  MkAString r1 ∨ MkAString r2 = MkAString $ Plus [r1,r2]

instance BoundedJoinSemilattice AString where
  bot = MkAString Zero

instance MeetSemilattice AString where
  MkAString r1 ∧ MkAString r2 = MkAString $ Regex.intersection r1 r2

instance BoundedMeetSemilattice AString where
  top = MkAString All

instance ComplementedLattice AString where
  neg (MkAString r) = MkAString $ Regex.complement r

instance Pretty AString where
  pretty (MkAString r) = 
    ann (Literal AbstractLit) $ case Regex.POSIX.ERE.fromRegex r of
      Nothing -> prettyRegex r
      Just re -> pretty $ Regex.POSIX.ERE.printERE re

-- TODO
pattern AString1 :: Text -> AString
pattern AString1 s <- (MkAString (regex1 -> Just s))

regex1 :: Regex -> Maybe Text
regex1 r0 = Text.pack <$> go [] r0
 where
  go xs (Times1 (Lit c) r) | [x] <- CS.toList c = go (x:xs) r
  go xs (Lit c)            | [x] <- CS.toList c = Just $ reverse (x:xs)
  go _ _                                        = Nothing

isIncludedBy :: AString -> AString -> Bool
isIncludedBy (MkAString r1) (MkAString r2)
  | Just b <- r1 `Regex.isUnambiguouslyIncludedBy` r2 = b
  | otherwise = r1 `Regex.isIncludedBy` r2

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
  | 0 <= m, m <= n = rep a m <> mconcat (replicate (fromIntegral $ n - m) (opt anyChar))
  | otherwise      = bot

anyChar :: AString
anyChar = MkAString AnyChar

star :: AString -> AString
star (MkAString r) = MkAString (Star r)

-- | r?
opt :: AString -> AString
opt (MkAString r) = MkAString (Opt r)

pattern Σ :: AString
pattern Σ = MkAString AnyChar

(⋅) :: AString -> AString -> AString
(⋅) = (<>)

------------------------------------------------------------------------------

member :: String -> AString -> Bool
member s (MkAString r) = Regex.membership s r

toChar :: AString -> Maybe AChar
toChar (MkAString r) = case Regex.simplify r of
  Lit c    -> Just (AChar.fromCharSet c)
  _        -> Nothing

toRegex :: AString -> Regex
toRegex (MkAString r) = r

toPOSIX :: AString -> Maybe Regex.POSIX.ERE.ERE
toPOSIX (MkAString r) = Regex.POSIX.ERE.fromRegex r

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

-- ⟦ ŝ[i] = c ⟧↑c ≐ ⋁{s∈ŝ} s[i]
charsAt :: AString -> Integer -> AChar
charsAt (MkAString r) i = 
  AChar.fromCharSet $ Regex.Type.charsAt (fromInteger i) r

-- ⟦ s[î..ĵ] = t ⟧↑t ≐ ⋁{i∈î and j∈ĵ} s[i..j]
strSub :: Text -> AInt -> AInt -> AString
strSub s a b = 
  joins [ eq $ Text.unpack $ Text.take (j - i + i) $ Text.drop i s
        | i <- map fromIntegral $ AInt.values $ (a ∧ safeBounds)
        , j <- map fromIntegral $ AInt.values $ (b ∧ safeBounds)
        ]
 where
  safeBounds = AInt.ge 0 ∧ AInt.lt (fromIntegral $ Text.length s)

------------------------------------------------------------------------------

strOfLen :: AInt -> AString
strOfLen (meet (AInt.ge 0) -> n̂)
  | isBot n̂ = bot
  | otherwise = joins $ AInt.intervals n̂ >>= \case
      Fin a :… Fin b  -> [rep Σ n | n <- [a..b]]
      Fin a :… PosInf -> [rep Σ a ⋅ star Σ]
      _               -> impossible

strNotOfLen :: AInt -> AString
strNotOfLen (meet (AInt.ge 0) -> n̂)
  | isBot n̂ = top
  | otherwise = meets $ AInt.intervals n̂ >>= \case
      Fin a :… Fin b  -> [ joins $ [rep Σ i | i <- [0..n-1]] ++
                                   [rep Σ (n + 1) ⋅ star Σ]
                         | n <- [a..b] ]
      Fin a :… PosInf -> [ joins $ [rep Σ i | i <- [0..a-1]] ]
      _               -> impossible

strWithCharAt :: AInt -> AChar -> AString
strWithCharAt (meet (AInt.ge 0) -> î) ĉ
  | isBot ĉ = strOfLen î  -- TODO: should this be min î ?
  | otherwise = joins $ AInt.intervals î >>= \case
      Fin a :… Fin b  -> [rep Σ i ⋅ lit ĉ ⋅ star Σ | i <- [a..b]]
      Fin a :… PosInf -> [rep Σ a ⋅ star Σ ⋅ lit ĉ ⋅ star Σ]
      _               -> impossible

strWithCharAtRev :: AInt -> AChar -> AString
strWithCharAtRev (meet (AInt.ge 1) -> î) ĉ
  | isBot ĉ = bot
  | otherwise = joins $ AInt.intervals î >>= \case
      Fin a :… Fin b  -> [star Σ ⋅ lit ĉ ⋅ rep2 Σ (a - 1) (b - 1)]
      Fin a :… PosInf -> [star Σ ⋅ lit ĉ ⋅ rep Σ (a - 1) ⋅ star Σ]
      _               -> impossible

strWithSubstr :: AInt -> AInt -> AString -> AString
strWithSubstr (meet (AInt.ge 0) -> î) (meet (AInt.geA î) -> ĵ) t̂
  | isBot î = bot
  | isBot ĵ = strOfLen î
  | otherwise = joins $ AInt.intervals î >>= \case
      Fin a :… Fin b -> AInt.intervals ĵ >>= \case
        Fin c :… Fin d  -> [str  i j | i <- [a..b], j <- [c..d]]
        Fin c :… PosInf -> [str' i c | i <- [a..b]]
        _               -> impossible
      Fin a :… PosInf -> AInt.intervals ĵ >>= \case
        Fin c :… Fin d  -> [str  a j | j <- [c..d], a <= j]
        Fin c :… PosInf -> [str' a c]
        _               -> impossible
      _ -> impossible
 where
  str  i j = rep Σ i ⋅  (rep Σ (j - i + 1)            ∧ t̂) ⋅ star Σ
  str' i j = rep Σ i ⋅ ((rep Σ (j - i + 1) ⋅ star Σ) ∧ t̂) ⋅ star Σ

strWithoutSubstr :: AInt -> AInt -> AString -> AString
strWithoutSubstr î ĵ t̂ = neg $ strWithSubstr î ĵ t̂

strWithFirstIndexOfChar :: AChar -> AInt -> AString
strWithFirstIndexOfChar ĉ î
  | isBot ĉ = strOfLen î -- TODO: should this be min î ?
  | Just m <- AInt.minimum î, m < Fin 0 
      = (star c̄) ∨ strWithFirstIndexOfChar ĉ (î ∧ AInt.ge 0)
  | otherwise = joins $ AInt.intervals î >>= \case
      Fin a :… Fin b  -> [rep c̄ i ⋅ lit ĉ ⋅ star Σ | i <- [a..b]]
      Fin a :… PosInf -> [rep c̄ a ⋅ star c̄ ⋅ lit ĉ ⋅ star Σ]
      _               -> impossible
 where
  c̄ = lit (neg ĉ)

strWithFirstIndexOfCharRev :: AChar -> AInt -> AString
strWithFirstIndexOfCharRev ĉ î
  | isBot ĉ = undefined -- TODO
  | Just m <- AInt.minimum î, m < Fin 1
      = (star c̄) ∨ strWithFirstIndexOfCharRev ĉ (î ∧ AInt.ge 1)
  | otherwise = joins $ AInt.intervals î >>= \case
      Fin a :… Fin b  -> [star c̄ ⋅ lit ĉ ⋅ rep2 Σ (a - 1) (b - 1)]
      Fin a :… PosInf -> [star c̄ ⋅ lit ĉ ⋅ rep Σ (a - 1) ⋅ star Σ]
      _               -> impossible
 where
  c̄ = lit (neg ĉ)

strWithFirstIndexOfCharFollowedByFirstIndexOfChar :: AChar -> AChar -> AInt -> AString
strWithFirstIndexOfCharFollowedByFirstIndexOfChar ĉ1 ĉ2 (meet (AInt.ge 1) -> î) -- TODO
  | not (isBot (ĉ1 ∧ ĉ2)) = undefined -- TODO
  | otherwise = joins $ AInt.intervals î >>= \case
      Fin a :… Fin b  -> [star c̄12 ⋅ lit ĉ1 ⋅ rep2 c̄2 a b ⋅ lit ĉ2 ⋅ star Σ]
      Fin a :… PosInf -> [star c̄12 ⋅ lit ĉ1 ⋅ rep c̄2 (a - 1) ⋅ star c̄2 ⋅ lit ĉ2 ⋅ star Σ]
      _               -> impossible
 where
  c̄12 = lit (neg ĉ1 ∧ neg ĉ2)
  c̄2  = lit (neg ĉ2)

strWithSubstrFromFirstIndexOfCharToEnd :: AChar -> Integer -> Integer -> AString -> AString
strWithSubstrFromFirstIndexOfCharToEnd ĉ i j t̂
  | i < 1 || j < 1 = undefined -- TODO
  | otherwise = star c̄ ⋅ lit ĉ ⋅ rep Σ (i-1) ⋅ t̂ ⋅ rep Σ (j-1)
 where
  c̄ = lit (neg ĉ)

strWithCharGapChar :: AChar -> Integer -> AChar -> AString
strWithCharGapChar a k b
  | k > 0     = star Σ ⋅ lit a ⋅ rep Σ (k - 1) ⋅ lit b ⋅ star Σ
  | k < 0     = star Σ ⋅ lit b ⋅ rep Σ (k - 1) ⋅ lit a ⋅ star Σ
  | otherwise = star Σ ⋅ lit (a ∧ b) ⋅ star Σ
