module Panini.Regex.Simplify where

import Algebra.Lattice
import Data.Containers.ListUtils
import Data.Generics.Uniplate.Direct
import Panini.Regex.CharSet qualified as CS
import Panini.Regex.Type
import Prelude

-- TODO: implement more complex simplifications à la Kahrs/Runciman 2022

-------------------------------------------------------------------------------

simplify :: Regex -> Regex
simplify = rewrite $ \case
  Lit a | [c] <- CS.toList a -> Just $ Word [c]

  Plus rs0 -> case filter (/= Zero) $ nubOrd rs0 of
    []                             -> Just Zero
    [r]                            -> Just r
    rs1 | any (== One) rs1         -> Just $ Opt $ Plus $ filter (/= One) rs1
        | any isOpt rs1            -> Just $ Opt $ Plus $ concatMap flatOpt rs1
        | all isLit rs1            -> Just $ Lit $ joins [a | Lit a <- rs1]
        | all isWord1 rs1          -> Just $ Lit $ joins [CS.singleton c | Word [c] <- rs1]
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
