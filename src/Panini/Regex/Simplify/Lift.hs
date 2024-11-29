module Panini.Regex.Simplify.Lift where

import Data.List.Extra (partition)
import Panini.Regex.CharSet (CharSet)
import Panini.Regex.CharSet qualified as CS
import Panini.Regex.Simplify.Common
import Panini.Regex.Type
import Prelude

(⊆) :: CharSet -> CharSet -> Bool
(⊆) = CS.isSubsetOf

lift :: Context -> Regex -> Regex
lift ctx r = case (ctx, r) of

  -- x* ⋅ y? = x*  if α(y) ⊆ α₁(x) ------------------------------
  (Free, Times xs) -> Times $ go xs
   where
    go (Star x : Star y : zs) | alpha y ⊆ alpha1 x = go (Star x : zs)
    go (Star x : Opt  y : zs) | alpha y ⊆ alpha1 x = go (Star x : zs)
    go (z:zs)                                      = z : go zs
    go []                                          = []

  -- x* + y = x*  if α(y) ⊆ α₁(x) -------------------------------
  (Free, Plus xs) -> Plus $ go xs
   where
    go (z:zs) = go1 z zs []
    go []     = []

    go1 (Star x) (y:ys) zs | alpha y ⊆ alpha1 x = go1 (Star x) ys zs
    go1 x (Star y : ys) zs | alpha x ⊆ alpha1 y = go (Star y : ys ++ zs)
    go1 x (y:ys) zs                             = go1 x ys (y:zs)
    go1 x []  zs                                = x : go zs

  -- (x + y)* = (α̂₁(x) + y)*  if α(x) ⊆ α₁(x + y) ---------------
  (Starred, Plus xs) -> 
    let a1 = alpha1 r in
    case partition (\x -> alpha x ⊆ a1) xs of
      ([], _ ) -> r
      (ys, zs) -> Plus $ map (Lit . alpha1) ys ++ zs

  -- (x⋅y?)* = x*  if α(y) ⊆ α₁(x) ------------------------------
  (Starred, TimesN x (Opt y)) | alpha y ⊆ alpha1 x -> x

  -- x* = α̂₁(x)*  if α(x) = α₁(x) -------------------------------
  (Starred, _) | alpha r == a1 -> Lit a1  where a1 = alpha1 r

  ---------------------------------------------------------------
  (Free,     _) -> r  
  (Optional, _) -> lift Free r
  (Starred,  _) -> lift Free r
