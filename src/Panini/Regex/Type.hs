{-
This module implements an efficient regular expression type.

Some aspects of note:

  1) Literals are represented as character sets ('CharSet') instead of just
     single characters ('Char'). This enables efficient and succinct
     representation of character classes (e.g., @[a-z]@ in POSIX syntax).
  
  2) Smart pattern synonyms ensure that 'Regex' instances are
     efficient-by-construction and uphold certain invariants, while still
     allowing natural deconstruction via pattern matching. The invariants are
     based on the standardisation rules from Kahrs and Runciman (2022).

References:

  * Kahrs, Stefan and Colin Runciman. 2022. "Simplifying Regular Expressions
    Further." Journal of Symbolic Computation 109 (2022): 124–143.
    https://doi.org/10.1016/j.jsc.2021.08.003

-}
module Panini.Regex.Type
  ( Regex(One,Lit)
  , pattern Zero
  , pattern AnyChar
  , pattern All
  , pattern Times
  , times
  , pattern Times1
  , unconsTimes
  , pattern TimesN
  , unsnocTimes
  , pattern Plus
  , plus
  , pattern Plus1
  , unconsPlus
  , deleteFindChoice
  , pattern Star
  , pattern Opt
  , nullable
  , minWordLength
  , maxWordLength
  ) where

import Data.Data (Data)
import Data.Foldable
import Data.Hashable
import Data.List.Extra (unsnoc)
import Data.Maybe
import Data.Semigroup (stimes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import GHC.Generics
import Panini.Panic
import Panini.Pretty
import Panini.Regex.CharSet (CharSet)
import Panini.Regex.CharSet qualified as CS
import Prelude

-- TODO: m-to-n-times repetition {m,n}
-- TODO: one-or-more repetition (+)

-------------------------------------------------------------------------------

-- | The 'Regex' type defines regular expressions over Unicode characters.
data Regex  
  = One           -- ^ identitity element (1), empty string (ε)
  | Lit !CharSet  -- ^ set of literal symbols, character class

  -- internal constructors; use pattern synonyms below
  | Plus_ !(Set Regex) !Bool
  | Times_ ![Regex] !Bool
  | Star_ !Regex
  | Opt_ !Regex

  deriving stock 
    ( Eq   -- ^ structural equivalence
    , Ord  -- ^ structural ordering    
    , Show, Read, Generic, Data
    )

-- | zero element (0), empty set (∅), bottom (⊥)
pattern Zero :: Regex
pattern Zero <- Lit (CS.null -> True) where
  Zero = Lit CS.empty
{-# INLINE Zero #-}

-- | set of all singleton words (Σ), class of all characters
pattern AnyChar :: Regex
pattern AnyChar <- Lit (CS.isFull -> True) where
  AnyChar = Lit CS.full
{-# INLINE AnyChar #-}

-- | set of all words (Σ*), top (⊤)
pattern All :: Regex
pattern All = Star AnyChar
{-# INLINE All #-}

-- | sequence (r₁ ⋅ r₂), concatenation (r₁ <> r₂)
--
-- Invariants:
--    1) Every sequence consists of at least two elements.
--    2) Sequences do not immediately contain other sequences.
--    3) Sequences do not immediately contain 'Zero' or 'One'.
--
pattern Times :: [Regex] -> Regex
pattern Times xs <- Times_ xs _ where
  Times xs = foldr times One xs
{-# INLINE Times #-}

times :: Regex -> Regex -> Regex
Zero         `times` _            = Zero
_            `times` Zero         = Zero
One          `times` r            = r
r            `times` One          = r
Times_ xs e1 `times` Times_ ys e2 = Times_ (xs ++ ys) (e1 && e2)
Times_ xs e  `times` r            = Times_ (xs ++ [r]) (e && nullable r)
r            `times` Times_ xs e  = Times_ (r:xs) (e && nullable r)
r1           `times` r2           = Times_ [r1,r2] (nullable r1 && nullable r2)
{-# INLINE times #-}

pattern Times1 :: Regex -> Regex -> Regex
pattern Times1 x y <- (unconsTimes -> Just (x,y))
{-# INLINE Times1 #-}

unconsTimes :: Regex -> Maybe (Regex, Regex)
unconsTimes = \case
  Times_ []     _ -> impossible
  Times_ [x,y]  _ -> Just (x, y)
  Times_ (x:xs) _ -> Just (x, Times_ xs (all nullable xs))
  _               -> Nothing
{-# INLINE unconsTimes #-}

pattern TimesN :: Regex -> Regex -> Regex
pattern TimesN x y <- (unsnocTimes -> Just (x,y))
{-# INLINE TimesN #-}

unsnocTimes :: Regex -> Maybe (Regex, Regex)
unsnocTimes = \case
  Times_ []                      _ -> impossible
  Times_ [x,y]                   _ -> Just (x, y)
  Times_ (unsnoc -> Just (xs,x)) _ -> Just (Times_ xs (all nullable xs),x)
  _                                -> Nothing
{-# INLINE unsnocTimes #-}

-- | choice (r₁ + r₂), alternation (r₁ | r₂), join (r₁ ∨ r₂)
--
-- Invariants:
--    1) Every choice consists of at least two elements.
--    2) Choices do not immediately contain other choices.
--    3) Choices do not immediately contain 'Zero' or 'One' or 'Opt'.
--    4) Choices do not immediately contain more than one set of literals.
--    5) Choices do not contain duplicates.
--    6) Choices are ordered (via 'Ord').
--
pattern Plus :: [Regex] -> Regex
pattern Plus xs <- Plus_ (Set.toAscList -> xs) _ where
  Plus xs = foldr plus Zero xs
{-# INLINE Plus #-}

plus :: Regex -> Regex -> Regex
Zero        `plus` r              = r
r           `plus` Zero           = r
One         `plus` r              = Opt r
r           `plus` One            = Opt r
Opt r1      `plus` r2             = Opt $ r1 `plus` r2
r1          `plus` Opt r2         = Opt $ r1 `plus` r2
Lit a       `plus` Lit b          = Lit (a <> b)
Plus_ xs e1 `plus` Plus_ ys e2    = Plus_ (mergeChoices xs ys) (e1 || e2)
Plus_ xs e  `plus` r              = Plus_ (insertChoice r xs) (e || nullable r)
r           `plus` Plus_ xs e     = Plus_ (insertChoice r xs) (e || nullable r)
r1          `plus` r2 | r1 < r2   = Plus_ (Set.fromDistinctAscList [r1,r2]) (nullable r1 || nullable r2)
                      | r1 > r2   = Plus_ (Set.fromDistinctAscList [r2,r1]) (nullable r1 || nullable r2)
                      | otherwise = r1
{-# INLINE plus #-}

mergeChoices :: Set Regex -> Set Regex -> Set Regex
mergeChoices xs ys 
  | Just (Lit a, xs') <- Set.minView xs
  , Just (Lit b, ys') <- Set.minView ys = Set.insert (Lit (a <> b)) (xs' <> ys')
  | otherwise = xs <> ys
{-# INLINE mergeChoices #-}

insertChoice :: Regex -> Set Regex -> Set Regex
insertChoice (Lit a) (Set.minView -> Just (Lit b, xs)) = Set.insert (Lit (a <> b)) xs
insertChoice x xs = Set.insert x xs
{-# INLINE insertChoice #-}

pattern Plus1 :: Regex -> Regex -> Regex
pattern Plus1 x y <- (unconsPlus -> Just (x,y))
{-# INLINE Plus1 #-}

unconsPlus :: Regex -> Maybe (Regex, Regex)
unconsPlus = \case  
  Plus_ xs0 _ -> case Set.minView xs0 of
    Nothing                            -> impossible
    Just (x,xs) | [y] <- Set.toList xs -> Just (x, y)
                | otherwise            -> Just (x, Plus_ xs (any nullable xs))
  _ -> Nothing
{-# INLINE unconsPlus #-}

deleteFindChoice :: Regex -> Regex -> Maybe Regex
deleteFindChoice x (Plus_ xs e) 
  | Set.member x xs, let xs' = Set.delete x xs 
  = Just $ Plus_ xs' (e && all nullable xs')
deleteFindChoice _ _ = Nothing

-- | iteration, Kleene closure (r*)
--
-- Invariants:
--    1) Iterations do not immediately contain other iterations.
--    2) Iterations do not immediately contain 'Zero' or 'One' or 'Opt'.
--
pattern Star :: Regex -> Regex
pattern Star x <- Star_ x where
  Star Zero     = One
  Star One      = One
  Star (Star x) = Star_ x
  Star (Opt x)  = Star_ x
  Star x        = Star_ x
{-# INLINE Star #-}

-- | option (r?)
--
-- Invariants:
--    1) Options do not immediately contain nullable expressions.
--    2) Options do not immediately contain 'Zero'.
--
pattern Opt :: Regex -> Regex
pattern Opt x <- Opt_ x where
  Opt Zero           = One
  Opt x | nullable x = x
        | otherwise  = Opt_ x
{-# INLINE Opt #-}

{-# COMPLETE One, Lit, Plus , Times , Star, Opt #-}
{-# COMPLETE One, Lit, Plus , Times1, Star, Opt #-}
{-# COMPLETE One, Lit, Plus1, Times , Star, Opt #-}
{-# COMPLETE One, Lit, Plus1, Times1, Star, Opt #-}

-------------------------------------------------------------------------------

instance Hashable Regex

instance IsString Regex where
  fromString = Times . map (Lit . CS.singleton)

instance Semigroup Regex where
  (<>) = times
  {-# INLINE (<>) #-}

  stimes 0 _ = One
  stimes 1 r = r
  stimes n r = foldr1 (<>) $ replicate (fromIntegral n) r

instance Monoid Regex where
  mempty = One
  {-# INLINE mempty #-}

-------------------------------------------------------------------------------

-- | A regex is /nullable/ if it accepts the empty string.
nullable :: Regex -> Bool
nullable = \case
  One          -> True
  Lit _        -> False
  Plus_ _ ewp  -> ewp
  Times_ _ ewp -> ewp
  Star_ _      -> True
  Opt_ _       -> True

-- | The length of the shortest word accepted by the regex, or 'Nothing' if the
-- regex is 'Zero'.
minWordLength :: Regex -> Maybe Int
minWordLength Zero = Nothing
minWordLength r0 = Just $ go r0
 where 
  go = \case
    One      -> 0
    Lit _    -> 1
    Plus rs  -> minimum $ map go rs
    Times rs -> sum $ map go rs
    Star _   -> 0
    Opt _    -> 0

-- | The length of the longest word accepted by the regex, or -1 if the regex
-- accepts an infinite language, or 'Nothing' if the regex is 'Zero'.
maxWordLength :: Regex -> Maybe Int
maxWordLength Zero = Nothing
maxWordLength r0 = Just $ fromMaybe (-1) $ go r0
 where
  go = \case
    One      -> Just 0
    Lit _    -> Just 1
    Plus rs  -> maximum <$> mapM go rs
    Times rs -> sum <$> mapM go rs
    Star _   -> Nothing
    Opt r    -> go r

-------------------------------------------------------------------------------

instance Pretty Regex where
  pretty = go (7 :: Int)
   where
    go p = \case
      Zero     -> emptySet
      One      -> epsilon
      Lit c    -> pretty c
      Plus rs  -> parensIf (p >= 7) $ concatWithOp "+" $ map (go 7) rs
      Times rs -> parensIf (p >= 8) $ mconcat $ map (go 8) rs
      Star r   -> parensIf (p >= 9) $ go 9 r <> "*" 
      Opt r    -> parensIf (p >= 9) $ go 9 r <> "?" 
