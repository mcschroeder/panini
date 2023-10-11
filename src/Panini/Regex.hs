module Panini.Regex where

import Algebra.Lattice
import Data.Generics.Uniplate.Direct
import Data.List qualified as List
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
-- TODO: plus
-- TODO: opt

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

opt :: Regex -> Regex
opt Zero           = One
opt One            = One
opt r | nullable r = r
opt r              = Opt r

star :: Regex -> Regex
star Zero     = One
star One      = One
star (Star r) = Star r
star r        = Star r

instance IsString Regex where
  fromString = Word

-- TODO: singleton literals
instance Semigroup Regex where
  Zero     <> _        = Zero
  _        <> Zero     = Zero
  One      <> r        = r
  r        <> One      = r
  Word a   <> Word b   = Word (a ++ b)
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
  r        ∨ r' | r == r' = r
  Zero     ∨ r            = r
  r        ∨ Zero         = r
  One      ∨ r            = opt r  
  r        ∨ One          = opt r
  Opt r1   ∨ r2           = opt (r1 ∨ r2)
  r1       ∨ Opt r2       = opt (r1 ∨ r2)
  Lit a    ∨ Lit b        = Lit (a ∨ b)
  Lit a    ∨ Word [b]     = Lit (a ∨ AChar.eq b)
  Word [a] ∨ Lit b        = Lit (AChar.eq a ∨ b)
  Word [a] ∨ Word [b]     = Lit (AChar.eq a ∨ AChar.eq b)
  Plus xs  ∨ Plus ys      = Plus $ List.nub (xs ++ ys)
  Plus xs  ∨ r            = if r `elem` xs then Plus xs else Plus (xs ++ [r])
  r        ∨ Plus xs      = if r `elem` xs then Plus xs else Plus (r:xs)
  r1       ∨ r2           = Plus [r1,r2]

instance BoundedJoinSemilattice Regex where
  bot = Zero

-------------------------------------------------------------------------------

instance Pretty Regex where
  pretty = \case
    Lit c -> pretty c
    Word s -> pretty s
    Plus rs -> parens $ concatWithOp "+" $ map pretty rs
    Times rs -> mconcat $ map pretty rs
    Star r@(Times _) -> parens (pretty r) <> "*"
    Star r@(Word _) -> parens (pretty r) <> "*"
    Star r -> pretty r <> "*"
    Opt r@(Times _) -> parens (pretty r) <> "?"
    Opt r@(Word _) -> parens (pretty r) <> "?"
    Opt r -> pretty r <> "?"

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


-- TODO: simplify explicitly (not implicitly via construction)
