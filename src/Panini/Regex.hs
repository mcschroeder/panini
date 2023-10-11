module Panini.Regex where

import Algebra.Lattice
import Data.Containers.ListUtils
import Data.Generics.Uniplate.Direct
import Data.Semigroup
import Data.String
import Panini.Abstract.AChar (AChar)
import Panini.Abstract.AChar qualified as AChar
import Panini.Pretty
import Prelude

-------------------------------------------------------------------------------

-- TODO
type CharSet = AChar

-------------------------------------------------------------------------------

-- | The 'Regex' type defines regular expressions over Unicode characters.
data Regex  
  = Lit CharSet   -- ^ set of literal symbols, character class
  | Word String   -- ^ word, sequence of singleton literals
  | Plus [Regex]  -- ^ choice (r₁ + r₂), alternation (r₁ | r₂), join (r₁ ∨ r₂)
  | Times [Regex] -- ^ sequence (r₁ ⋅ r₂), concatenation (r₁ <> r₂)
  | Star Regex    -- ^ iteration, Kleene closure (r*)
  | Opt Regex     -- ^ option (r?)
  deriving stock (Eq, Ord, Show, Read)
-- TODO: n-times repetition
-- TODO: Plus (+)

-- | zero element (0), empty set (∅), bottom (⊥)
pattern Zero :: Regex
pattern Zero <- Lit (isBot -> True) where
  Zero = Lit bot

-- | identitity element (1), empty string (ε)
pattern One :: Regex
pattern One = Word ""

-- | set of all singleton words (Σ), class of all characters
pattern AnyChar :: Regex
pattern AnyChar <- Lit (isTop -> True) where
  AnyChar = Lit top

-- | set of all words (Σ*), top (⊤)
pattern All :: Regex
pattern All = Star AnyChar

instance IsString Regex where
  fromString = Word

instance Semigroup Regex where
  Times xs <> Times ys = Times (xs ++ ys)
  Times xs <> r        = Times (xs ++ [r])
  r        <> Times xs = Times (r:xs)
  r1       <> r2       = Times [r1,r2]

  stimes 0 _ = One
  stimes 1 r = r
  stimes n r = foldr1 (<>) $ replicate (fromIntegral n) r

instance Monoid Regex where
  mempty = One

instance JoinSemilattice Regex where
  Plus rs1 ∨ Plus rs2 = Plus (rs1 ++ rs2)
  Plus rs  ∨ r        = Plus (rs ++ [r])
  r        ∨ Plus rs  = Plus (r:rs)
  r1       ∨ r2       = Plus [r1,r2]

instance BoundedJoinSemilattice Regex where
  bot = Zero

-------------------------------------------------------------------------------

instance Pretty Regex where
  pretty = \case
    Lit c -> pretty c
    Word [] -> "ε"
    Word s -> ann (Literal StringLit) $ pretty s
    Plus rs -> parens $ concatWithOp "+" $ map pretty rs
    Times rs -> parens $ concatWithOp "⋅" $ map pretty rs
    Star r -> parens (pretty r) <> "*"
    Opt r -> parens (pretty r) <> "?"

instance Uniplate Regex where
  uniplate = \case
    Lit c    -> plate Lit |- c
    Word s   -> plate Word |- s
    Plus rs  -> plate Plus ||* rs
    Times rs -> plate Times ||* rs
    Star r   -> plate Star |* r
    Opt r    -> plate Opt |* r

-------------------------------------------------------------------------------

-- | A regex is /nullable/ if it accepts the empty string.
nullable :: Regex -> Bool
nullable = \case
  Lit _    -> False
  Word s   -> null s
  Plus rs  -> or $ map nullable rs
  Times rs -> and $ map nullable rs
  Star _   -> True
  Opt _    -> True

-------------------------------------------------------------------------------

-- TODO: implement more complex simplifications à la Kahrs/Runciman 2022

simplify :: Regex -> Regex
simplify = rewrite $ \case
  Lit a | [c] <- AChar.values a -> Just $ Word [c]

  Plus rs0 -> case nubOrd rs0 of
    []                             -> Just Zero
    [r]                            -> Just r
    rs1 | any (== One) rs1         -> Just $ Opt $ Plus $ filter (== One) rs1
        | any isOpt rs1            -> Just $ Opt $ Plus $ concatMap flatOpt rs1
        | all isLit rs1            -> Just $ Lit $ joins [a | Lit a <- rs1]
        | all isWord1 rs1          -> Just $ Lit $ joins [AChar.eq c | Word [c] <- rs1]
        | any isPlus rs1           -> Just $ Plus $ concatMap flatPlus rs1
        | length rs1 /= length rs0 -> Just $ Plus rs1
        | otherwise                -> Nothing

  Times rs0 -> case filter (/= One) rs0 of
    []                             -> Just One
    [r]                            -> Just r
    rs1 | any (== Zero) rs1        -> Just Zero
        | all isWord rs1           -> Just $ Word $ concat [s | Word s <- rs1]
        | any isTimes rs1          -> Just $ Times $ concatMap flatTimes rs1
        | length rs1 /= length rs0 -> Just $ Times rs1
        | otherwise                -> Nothing

  Star Zero     -> Just One
  Star One      -> Just One
  Star (Star r) -> Just $ Star r
  
  Opt Zero           -> Just One
  Opt One            -> Just One
  Opt r | nullable r -> Just r

  _ -> Nothing

isLit :: Regex -> Bool
isLit (Lit _) = True
isLit _       = False
{-# INLINE isLit #-}

isWord :: Regex -> Bool
isWord (Word _) = True
isWord _        = False
{-# INLINE isWord #-}

isWord1 :: Regex -> Bool
isWord1 (Word [_]) = True
isWord1 _          = False
{-# INLINE isWord1 #-}

isTimes :: Regex -> Bool
isTimes (Times _) = True
isTimes _         = False
{-# INLINE isTimes #-}

isPlus :: Regex -> Bool
isPlus (Plus _) = True
isPlus _        = False
{-# INLINE isPlus #-}

isOpt :: Regex -> Bool
isOpt (Opt _) = True
isOpt _       = False
{-# INLINE isOpt #-}

flatTimes :: Regex -> [Regex]
flatTimes (Times xs) = xs
flatTimes x          = [x]
{-# INLINE flatTimes #-}

flatPlus :: Regex -> [Regex]
flatPlus (Plus xs) = xs
flatPlus x         = [x]
{-# INLINE flatPlus #-}

flatOpt :: Regex -> [Regex]
flatOpt (Opt r) = [r]
flatOpt x       = [x]
{-# INLINE flatOpt #-}
