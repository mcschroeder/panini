module Regex.Simplify.Common where

import Data.List.Extra (uncons)
import Prelude
import Regex.CharSet (CharSet)
import Regex.Type

-- | Simplification context (essentially, the kind of surrounding regex).
data Context = Free | Optional | Starred
  deriving stock (Eq, Ord, Show)

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

flatNullable :: Regex -> Regex
flatNullable = \case
    Star r -> r
    Opt  r -> r
    r      -> r

-- TODO: replace by Data.List.unsnoc once we depend on base-4.19
unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs = (\(hd, tl) -> (reverse tl, hd)) <$> uncons (reverse xs)
