module Panini.Regex.Simplify.Fuse where

import Panini.Regex.Simplify.Common
import Panini.Regex.Type
import Prelude

fuse :: Context -> Regex -> Regex
fuse ctx r = case (ctx, r) of
  -- x*x* = x*x? = x?x* = x* ------------------------------------
  (Free, Times xs) -> Times $ go xs
   where
    go (Star x1 : Star x2 : ys) | x1 == x2 = go (Star x1 : ys)
    go (Star x1 : Opt  x2 : ys) | x1 == x2 = go (Star x1 : ys)
    go (Opt  x1 : Star x2 : ys) | x1 == x2 = go (Star x1 : ys)
    go (y:ys) = y : go ys
    go [] = []

  -- (x*⋅x)? = (x⋅x*)? = (x*)? ----------------------------------
  (Optional, Times1 (Star x1) x2) | x1 == x2 -> Star x1
  (Optional, TimesN x1 (Star x2)) | x1 == x2 -> Star x1

  -- (x⋅(x?⋅y)?)? = x?⋅(x⋅y)? -----------------------------------
  (Optional, TimesN x1 (Opt (Times1 (Opt x2) y))) 
    | x1 == x2 -> Opt x1 <> Opt (x1 <> y)
  
  -- ((x?⋅y)?⋅x)? = x?⋅(y⋅x)? -----------------------------------
  (Optional, Times1 (Opt (Times1 (Opt x2) y)) x1)
    | x1 == x2 -> Opt x1 <> Opt (y <> x1)

  -- (x⋅(y⋅x?)?)? = (x⋅y)?⋅x? -----------------------------------
  (Optional, TimesN x1 (Opt (Times1 y (Opt x2)))) 
    | x1 == x2 -> Opt (x1 <> y) <> Opt x1
  
  -- ((y⋅x?)?⋅x)? = (y⋅x)?⋅x? -----------------------------------
  (Optional, Times1 (Opt (Times1 y (Opt x2))) x1)
    | x1 == x2 -> Opt (y <> x1) <> Opt x1

  -- (x*⋅(x+y))? = x*⋅y? ----------------------------------------
  (Optional, Times1 (Star x) ys) 
    | Just ys' <- deleteFindChoice x ys -> Star x <> Opt ys'

  -- ((x+y)⋅x*)? = y?⋅x* ----------------------------------------
  (Optional, Times1 ys (Star x)) 
    | Just ys' <- deleteFindChoice x ys -> Opt ys' <> Star x

  -- (x⋅(x* + y))? = x* + x⋅y -----------------------------------
  (Optional, TimesN x ys)
    | Just ys' <- deleteFindChoice (Star x) ys -> (Star x) `plus` (x <> ys')

  -- ((x* + y)⋅x)? = x* + y⋅x -----------------------------------
  (Optional, Times1 ys x)
    | Just ys' <- deleteFindChoice (Star x) ys -> (Star x) `plus` (ys' <> x)
  
  -- (x?y?)* = (x+y?)* = (x+y*)* = (x+y)* -----------------------
  (Starred, Times xs) | nullable r -> Plus $ map flatNullable xs
  (Starred, Plus xs)               -> Plus $ map flatNullable xs
    
  ---------------------------------------------------------------
  (Free,     _) -> r
  (Optional, _) -> fuse Free r
  (Starred,  _) -> fuse Optional r
