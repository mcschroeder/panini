{-
This module implements regular expression simplifications based on the
transformations from Kahrs and Runcimcan (2022).

References:

  * Kahrs, Stefan and Colin Runciman. 2022. "Simplifying Regular Expressions
    Further." Journal of Symbolic Computation 109 (2022): 124–143.
    https://doi.org/10.1016/j.jsc.2021.08.003

-}
module Panini.Regex.Simplify (simplify) where

import Data.Function
import Data.List (partition, sortBy)
import Panini.Regex.CharSet (CharSet)
import Panini.Regex.CharSet qualified as CS
import Panini.Regex.Type
import Prelude

-------------------------------------------------------------------------------

simplify :: Regex -> Regex
simplify = goFree
 where
  goFree r = case r of
    Plus xs
      | r' <- factorChoices xs, r' /= r -> goFree r'
      | r' <- Plus (map goFree xs), r' /= r -> goFree r'
    Times xs
      | r' <- fuseSequence xs, r' /= r -> goFree r'
      | r' <- Times (map goFree xs), r' /= r -> goFree r'
    Star x
      | r' <- Star (goStar x), r' /= r -> goFree r'    
    Opt x
      | r' <- Opt (goOpt x), r' /= r -> goFree r'
    x -> x
  
  goOpt r = case r of
    Times xs
      | r' <- fuseOptSequence xs, r' /= r -> goOpt r'
    x -> goFree x

  goStar r = case r of
    Plus xs
      | r' <- fuseStarChoices xs, r' /= r -> goStar r'
      | r' <- liftStarChoices xs, r' /= r -> goStar r'
    Times xs
      | r' <- fuseStarSequence xs, r' /= r -> goStar r'
    x 
      | alpha x == alpha1 x -> Lit $ alpha1 x
      | otherwise -> goOpt x

-------------------------------------------------------------------------------

-- | Apply the free fusion law on a sequence of regular expressions.
--
--    x* ⋅ x* = x* ⋅ x? = x? ⋅ x* = c*
--
fuseSequence :: [Regex] -> Regex
fuseSequence = Times . go
 where
  go (Star x1 : Star x2 : ys) | x1 == x2 = go (Star x1 : ys)
  go (Star x1 : Opt  x2 : ys) | x1 == x2 = go (Star x1 : ys)
  go (Opt  x1 : Star x2 : ys) | x1 == x2 = go (Star x1 : ys)
  go (y:ys) = y : go ys
  go [] = []

-- | Apply the starred fusion law on a sequence of regular expressions.
--
--    (x? ⋅ y?)* = (x + y)*
--
fuseStarSequence :: [Regex] -> Regex
fuseStarSequence xs
  | all nullable xs = Plus $ map flatNullable xs
  | otherwise       = Times xs

-- | Apply the starred fusion law on a list of choices.
--
--    (x + y?)* = (x + y*)* = (x + y)*
--
fuseStarChoices :: [Regex] -> Regex
fuseStarChoices = Plus . map flatNullable

-- | Apply fusion rules on an optional sequence of expression.
--
--    (xx*)? = (x*)?
--    (x*x)? = (x*)?
--
fuseOptSequence :: [Regex] -> Regex
fuseOptSequence = \case
  [x1, Star x2] | x1 == x2 -> Star x2
  [Star x1, x2] | x1 == x2 -> Star x1
  xs -> Times xs

flatNullable :: Regex -> Regex
flatNullable = \case
    Star r -> r
    Opt  r -> r
    r      -> r

-------------------------------------------------------------------------------

-- | Factorize an ordered list of choices by pairwise application of the
-- distributive laws, choosing the smallest overall outcome.
--
--     (a⋅x)+(a⋅y) = a⋅(x+y)
--     (x⋅b)+(y⋅b) = (x+y)⋅b
--
factorChoices :: [Regex] -> Regex
factorChoices xs = smaller
  (apply (pairwise factorPrefix) xs)
  (apply (pairwise factorSuffix . sortBy (compare `on` reverse)) xs)
 where
  apply f = Plus . map Times . f . map flatTimes
  
  factorPrefix ax ay = case splitPrefix ax ay of
    ([], _, _) -> Nothing
    (a , x, y) -> Just $ a ++ [Plus [Times x, Times y]]

  factorSuffix xb yb = case splitSuffix xb yb of
    (_, _, []) -> Nothing
    (x, y, b ) -> Just $ Plus [Times x, Times y] : b

flatTimes :: Regex -> [Regex]
flatTimes = \case
  Times xs -> xs
  x        -> [x]

-- | Split two lists at the longest common prefix.
--
-- >>> splitPrefix "mouse" "moose"
-- ("mo", "use", "ose")
--
splitPrefix :: Eq a => [a] -> [a] -> ([a], [a], [a])
splitPrefix = go []
 where
  go zs (x:xs) (y:ys) | x == y = go (x:zs) xs ys
  go zs xs ys = (reverse zs, xs, ys)

-- | Split two lists at the longest common suffix.
--
-- >>> splitSuffix "mouse" "moose"
-- ("mou", "moo", "se")
--
splitSuffix :: Eq a => [a] -> [a] -> ([a], [a], [a])
splitSuffix xs0 ys0 = go [] (reverse xs0) (reverse ys0)
 where
  go zs (x:xs) (y:ys) | x == y = go (x:zs) xs ys
  go zs xs ys = (reverse xs, reverse ys, zs)

-- | Apply a function pairwise to a list of values, reducing them if possible,
-- otherwise continuing on.
pairwise :: (a -> a -> Maybe a) -> [a] -> [a]
pairwise _ [] = []
pairwise _ [x] = [x]
pairwise f (x:y:zs) = case f x y of
  Just z -> pairwise f (z:zs)
  Nothing -> x : pairwise f (y:zs)

-------------------------------------------------------------------------------

-- | Apply the the generalized lifting law on a starred list of choices.
--
--     (x + y)* = (α̂₁(x) + y)*  if α(x) ⊆ α₁(x + y)
--
liftStarChoices :: [Regex] -> Regex
liftStarChoices xs = case partition (\x -> alpha x `CS.isSubsetOf` a1) xs of
  ([], _ ) -> Plus xs
  (ys, zs) -> Plus $ map (Lit . alpha1) ys ++ zs
 where
  a1 = alpha1 (Plus xs)

-------------------------------------------------------------------------------

-- | Choose the smaller of two regular expressions in terms of 'size'.
smaller :: Regex -> Regex -> Regex
smaller r1 r2 
  | size r1 <= size r2 = r1
  | otherwise          = r2

-- | The size of a regular expression, following Kahrs and Runciman (2021).
size :: Regex -> Int
size = \case
  Zero     -> 0
  One      -> 0
  Lit _    -> 1
  Plus xs  -> sum (map size xs) + (length xs - 1)
  Times xs -> sum (map size xs) + (length xs - 1)
  Star x   -> size x + 1
  Opt x    -> size x + 1

-------------------------------------------------------------------------------

-- | The alphabet α(r) of characters that appear in the language L(r) of the
-- regular expression r.
alpha :: Regex -> CharSet
alpha = \case
  One      -> mempty
  Lit c    -> c
  Plus rs  -> mconcat $ map alpha rs
  Times rs -> mconcat $ map alpha rs
  Star r   -> alpha r
  Opt r    -> alpha r

-- | The alphabet α₁(r) of characters that appear as singleton words in the
-- language L(r) of the regular expression r.
alpha1 :: Regex -> CharSet
alpha1 = \case
  One      -> mempty
  Lit c    -> c
  Plus rs  -> mconcat $ map alpha1 rs
  Times rs -> timesAlpha1 rs
  Star r   -> alpha1 r
  Opt r    -> alpha1 r

-- | The alphabet α₁(r) of characters that appear as singleton words in the
-- language L(r) of the concatenation r = r₁⋅r₂⋯rₙ of the regular expressions
-- r₁,r₂,…,rₙ.
timesAlpha1 :: [Regex] -> CharSet
timesAlpha1 = fst . foldr go (mempty, True)
 where
  -- the bool indicates whether the tail of the concatenation is nullable
  go r (a, True ) | nullable r = (a <> alpha1 r, True  )
                  | otherwise  = (     alpha1 r, False )
  go r (a, False) | nullable r = (a            , False )
                  | otherwise  = (mempty       , False )

