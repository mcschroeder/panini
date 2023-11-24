module Panini.Regex.Simplify (simplify) where

import Algebra.Lattice
import Data.Generics.Uniplate.Direct
import Panini.Regex.Type
import Prelude

-- TODO: implement more complex simplifications à la Kahrs/Runciman 2022

-------------------------------------------------------------------------------

simplify :: Regex -> Regex
simplify = rewrite $ \case
  Plus rs
    | all isLit rs   -> Just $ Lit $ joins [a | Lit a <- rs]

  _ -> Nothing

isLit :: Regex -> Bool
isLit (Lit _) = True
isLit _       = False
{-# INLINE isLit #-}
