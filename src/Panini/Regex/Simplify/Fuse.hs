module Panini.Regex.Simplify.Fuse where

import Panini.Regex.Simplify.Common
import Panini.Regex.Type
import Prelude

fuse :: Context -> Regex -> Regex
fuse ctx r = case (ctx, r) of
  -- x*x* = x*x? = x?x* = x*
  (Free, Times xs) -> Times $ dropStars xs
  (Free, _)        -> r

  -- (x*⋅x)? = (x⋅x*)? = (x*)?
  (Optional, Times1 (Star x1) x2) | x1 == x2 -> Star x1
  (Optional, TimesN x1 (Star x2)) | x1 == x2 -> Star x1
  (Optional, _)                              -> fuse Free r
  
  -- (x?y?)* = (x+y?)* = (x+y*)* = (x+y)*
  (Starred, Times xs) | nullable r -> Plus $ map flatNullable xs
  (Starred, Plus xs)               -> Plus $ map flatNullable xs
  (Starred, _)                     -> fuse Optional r

dropStars :: [Regex] -> [Regex]
dropStars = go
 where
  go (Star x1 : Star x2 : ys) | x1 == x2 = go (Star x1 : ys)
  go (Star x1 : Opt  x2 : ys) | x1 == x2 = go (Star x1 : ys)
  go (Opt  x1 : Star x2 : ys) | x1 == x2 = go (Star x1 : ys)
  go (y:ys) = y : go ys
  go [] = []
