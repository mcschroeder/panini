module Panini.Regex.Simplify (simplify) where

import Algebra.Lattice
import Data.Generics.Uniplate.Direct
import Panini.Regex.CharSet qualified as CS
import Panini.Regex.Type
import Prelude

-- TODO: implement more complex simplifications à la Kahrs/Runciman 2022

-------------------------------------------------------------------------------

simplify :: Regex -> Regex
simplify = rewrite $ \case
  Lit a | [c] <- CS.toList a -> Just $ Word [c]

  Plus rs
    | all isLit rs   -> Just $ Lit $ joins [a | Lit a <- rs]
    | all isWord1 rs -> Just $ Lit $ joins [CS.singleton c | Word [c] <- rs]

  Times rs
    | all isWord rs -> Just $ Word $ concat [s | Word s <- rs]

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
