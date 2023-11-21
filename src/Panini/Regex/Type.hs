{-
This module implements an efficient regular expression type.

Some aspects of note:

  1) Literals are represented as character sets ('CharSet') instead of just
     single characters ('Char'). This enables efficient and succinct
     representation of character classes (e.g., @[a-z]@; see also
     "Panini.Regex.POSIX").
  
  2) Smart pattern synonyms ensure that 'Regex' instances are
     efficient-by-construction and uphold certain invariants, while still
     allowing natural deconstruction via pattern matching.

-}
module Panini.Regex.Type
  ( Regex(Lit,Word)
  , pattern Zero
  , pattern One
  , pattern AnyChar
  , pattern All
  , pattern Times
  , pattern Plus
  , pattern Star
  , pattern Opt  
  , nullable
  ) where

import Algebra.Lattice
import Data.Generics.Uniplate.Direct
import Data.Hashable
import Data.Semigroup hiding (All)
import Data.Set qualified as Set
import Data.String
import GHC.Generics
import Panini.Regex.CharSet (CharSet)
import Prelude

-- TODO: m-to-n-times repetition {m,n}
-- TODO: one-or-more repetition (+)

-------------------------------------------------------------------------------

-- | The 'Regex' type defines regular expressions over Unicode characters.
data Regex  
  = Lit CharSet   -- ^ set of literal symbols, character class
  | Word String   -- ^ word, sequence of singleton literals
  | Plus_ [Regex]
  | Times_ [Regex] 
  | Star_ Regex
  | Opt_ Regex
  deriving stock (Eq, Ord, Show, Read, Generic)

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

-- | sequence (r₁ ⋅ r₂), concatenation (r₁ <> r₂)
--
-- Invariants:
--    1) Every sequence consists of at least two elements.
--    2) Sequences do not contain other sequences.
--    3) Sequences do not contain 'Zero' or 'One'.
--
pattern Times :: [Regex] -> Regex
pattern Times xs <- Times_ xs where
  Times xs | elem Zero xs' = Zero
           | null xs'      = One
           | [r] <- xs'    = r
           | otherwise     = Times_ xs
   where
    xs' = concatMap flatTimes xs
    flatTimes = \case
      Times ys -> ys
      One      -> []
      y        -> [y]

-- | choice (r₁ + r₂), alternation (r₁ | r₂), join (r₁ ∨ r₂)
--
-- Invariants:
--    1) Every choice consists of at least two elements.
--    2) Choices do not contain other choices.
--    3) Choices do not contain 'Zero' or 'One' or 'Opt'.
--    3) Choices do not contain duplicates.
--    4) Choices are ordered (via 'Ord').
--
pattern Plus :: [Regex] -> Regex
pattern Plus xs <- Plus_ xs where
  Plus xs | any nullable xs && not (any nullable xs') = Opt (Plus_ xs')
          | null xs'   = Zero
          | [r] <- xs' = r          
          | otherwise  = Plus_ xs'
   where
    xs' = Set.toAscList $ Set.fromList $ concatMap flatPlus xs
    flatPlus = \case
      Plus ys -> ys
      Zero    -> []
      One     -> []
      Opt y   -> flatPlus y
      y       -> [y]

-- | iteration, Kleene closure (r*)
--
-- Invariants:
--    1) Iterations do not contain other iterations.
--    2) Iterations do not contain 'Zero' or 'One' or 'Opt'.
--
pattern Star :: Regex -> Regex
pattern Star x <- Star_ x where
  Star Zero     = One
  Star One      = One
  Star (Star x) = Star_ x
  Star (Opt x)  = Star_ x
  Star x        = Star_ x

-- | option (r?)
--
-- Invariants:
--    1) Options do not contain nullable expressions.
--    2) Options do not contain 'Zero'
--
pattern Opt :: Regex -> Regex
pattern Opt x <- Opt_ x where
  Opt Zero           = One
  Opt x | nullable x = x
        | otherwise  = Opt_ x

{-# COMPLETE Lit, Word, Plus, Times, Star, Opt #-}

-------------------------------------------------------------------------------

instance Hashable Regex

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
