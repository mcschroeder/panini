module Panini.Regex where

import Algebra.Lattice
import Panini.Abstract.AChar (AChar)
import Prelude
import Data.String
import Data.List qualified as List


-- TODO
type CharSet = AChar


-- TODO: n-times repetition
-- TODO: optional
data Regex
  = One
  | Lit CharSet
  | Word String
  | Plus [Regex]   -- non-empty, ordered, unique
  | Times [Regex]  -- non-empty, ordered, unique
  | Star Regex
  deriving stock (Eq, Ord, Show, Read)

pattern Zero :: Regex
pattern Zero <- Lit (isBot -> True) where
  Zero = Lit bot

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

instance Monoid Regex where
  mempty = One

instance JoinSemilattice Regex where
  Zero    ∨ Plus xs = Plus xs
  Plus xs ∨ Zero    = Plus xs
  Lit a   ∨ Lit b   = Lit (a ∨ b)
  Plus xs ∨ Plus ys = mkPlus (xs ++ ys)
  Plus xs ∨ r       = mkPlus (xs ++ [r])
  r       ∨ Plus xs = mkPlus (r:xs)
  r1      ∨ r2      = Plus [r1,r2]

mkPlus :: [Regex] -> Regex
mkPlus = Plus . List.nub
