module Panini.Regex.Simplify.Factor where

import Data.Function
import Data.List.Extra (sortBy, splitAtEnd, breakEnd)
import Panini.Regex.Simplify.Common
import Panini.Regex.Type
import Prelude

factor :: Context -> Regex -> Regex
factor _ = \case
  Plus xs -> Plus $ factorize xs
  r       -> r

factorize :: [Regex] -> [Regex]
factorize = facSuf . sortBy (compare `on` (reverse . flatTimes)) . facPre

facPre :: [Regex] -> [Regex]
facPre []                  = []
facPre [r]                 = [r]
facPre (r1:r2:rest)
  
  -- a⋅x + a⋅y = a⋅(x+y) ----------------------------------------
  | ax                    <- flatTimes r1
  , ay                    <- flatTimes r2
  , axy                   <- splitPrefix ax ay
  , (a,x,y)               <- map3 Times axy               
  , a /= One               = facPre $ a <> Plus [x,y] : rest

  -- (a⋅b)?⋅x + a⋅y = x + a⋅(b⋅x + y) ---------------------------
  | Opt ab : x1           <- flatTimes r1
  , abx                   <- flatTimes ab ++ x1
  , ay                    <- flatTimes r2  
  , (a,bx,y)              <- map3 Times $ splitPrefix abx ay
  , x                     <- Times x1
  , a /= One
  , bx /= One
  , size a + 1 >= size x   = facPre $ x : a <> Plus [bx,y] : rest

  -- a⋅x + (a⋅b)?⋅y = y + a⋅(x + b⋅y) ---------------------------
  | Opt ab : y1           <- flatTimes r2
  , aby                   <- flatTimes ab ++ y1
  , ax                    <- flatTimes r1
  , (a,x,by)              <- map3 Times $ splitPrefix ax aby
  , y                     <- Times y1
  , a /= One
  , by /= One
  , size a + 1 >= size y   = facPre $ y : a <> Plus [x,by] : rest
  
  -- a*⋅a⋅x + a⋅y = a⋅(a*⋅x + y) --------------------------------
  | Star a1 : ax1         <- flatTimes r1
  , a2                    <- flatTimes a1
  , (a3,x1)               <- splitAt (length a2) ax1
  , a2 == a3
  , ax                    <- a3 ++ Star a1 : x1
  , ay                    <- flatTimes r2
  , axy                   <- splitPrefix ax ay
  , (a,x,y)               <- map3 Times axy
  , a /= One               = facPre $ a <> Plus [x,y] : rest
  
  -- a⋅x + a*⋅a⋅y = a⋅(x + a*⋅y) --------------------------------
  | Star a1 : ay1         <- flatTimes r2
  , a2                    <- flatTimes a1
  , (a3,y1)               <- splitAt (length a2) ay1
  , a2 == a3
  , ay                    <- a3 ++ Star a1 : y1
  , ax                    <- flatTimes r1
  , axy                   <- splitPrefix ax ay
  , (a,x,y)               <- map3 Times axy
  , a /= One               = facPre $ a <> Plus [x,y] : rest
  
  -- a⋅a*⋅x + a*⋅y = a*⋅(a⋅x + y) -------------------------------
  | ax1                   <- flatTimes r1
  , (a1, Star a2 : x1)    <- break isStar ax1
  , a1 == flatTimes a2
  , ax                    <- Star a2 : a1 ++ x1
  , ay                    <- flatTimes r2
  , axy                   <- splitPrefix ax ay
  , (a,x,y)               <- map3 Times axy
  , a /= One               = facPre $ a <> Plus [x,y] : rest
  
  -- a*⋅x + a⋅a*⋅y = a*⋅(x + a⋅y) -------------------------------
  | ay1                   <- flatTimes r2
  , (a1, Star a2 : y1)    <- break isStar ay1
  , a1 == flatTimes a2
  , ay                    <- Star a2 : a1 ++ y1
  , ax                    <- flatTimes r2
  , axy                   <- splitPrefix ax ay
  , (a,x,y)               <- map3 Times axy
  , a /= One               = facPre $ a <> Plus [x,y] : rest

  ---------------------------------------------------------------
  | otherwise              = r1 : facPre (r2 : rest)


facSuf :: [Regex] -> [Regex]
facSuf []                  = []
facSuf [r]                 = [r]
facSuf (r1:r2:rest)

  -- x⋅b + y⋅b = (x+y)⋅b ----------------------------------------
  | xb                    <- flatTimes r1
  , yb                    <- flatTimes r2
  , xyb                   <- splitSuffix xb yb
  , (x,y,b)               <- map3 Times xyb
  , b /= One               = facSuf $ Plus [x,y] <> b : rest
  
  -- x⋅(a⋅b)? + y⋅b = x + (x⋅a + y)⋅b ---------------------------
  | Just (x1, Opt ab)     <- unsnoc $ flatTimes r1
  , xab                   <- x1 ++ flatTimes ab
  , yb                    <- flatTimes r2
  , xayb                  <- splitSuffix xab yb
  , (xa,y,b)              <- map3 Times xayb
  , x                     <- Times x1
  , b /= One
  , xa /= One
  , size b + 1 >= size x   = facSuf $ x : Plus [xa,y] <> b : rest
  
  -- x⋅b + y⋅(a⋅b)? = y + (x + y⋅a)⋅b ---------------------------
  | Just (y1, Opt ab)     <- unsnoc $ flatTimes r2
  , yab                   <- y1 ++ flatTimes ab
  , xb                    <- flatTimes r1
  , xyab                  <- splitSuffix xb yab
  , (x,ya,b)              <- map3 Times xyab
  , y                     <- Times y1
  , b /= One
  , ya /= One
  , size b + 1 >= size y   = facSuf $ y : Plus [x,ya] <> b : rest
  
  -- x⋅b⋅b* + y⋅b = (x⋅b* + y)⋅b --------------------------------
  | xb1                   <- flatTimes r1
  , Just (xb2, Star b1)   <- unsnoc xb1
  , b2                    <- flatTimes b1
  , (x1,b3)               <- splitAtEnd (length b2) xb2
  , b2 == b3
  , xb                    <- x1 ++ Star b1 : b3
  , yb                    <- flatTimes r2
  , xyb                   <- splitSuffix xb yb
  , (x,y,b)               <- map3 Times xyb
  , b /= One               = facSuf $ Plus [x,y] <> b : rest
  
  -- x⋅b + y⋅b⋅b* = (x + y⋅b*)⋅b --------------------------------
  | yb1                   <- flatTimes r2
  , Just (yb2, Star b1)   <- unsnoc yb1
  , b2                    <- flatTimes b1
  , (y1,b3)               <- splitAtEnd (length b2) yb2
  , b2 == b3
  , yb                    <- y1 ++ Star b1 : b3
  , xb                    <- flatTimes r1
  , xyb                   <- splitSuffix xb yb
  , (x,y,b)               <- map3 Times xyb
  , b /= One               = facSuf $ Plus [x,y] <> b : rest
  
  -- x⋅b*⋅b + y⋅b* = (x⋅b + y)⋅b* -------------------------------
  | xb1                   <- flatTimes r1
  , (xb2, b1)             <- breakEnd isStar xb1
  , Just (x1, Star b2)    <- unsnoc xb2
  , b1 == flatTimes b2
  , xb                    <- x1 ++ b1 ++ [Star b2]
  , yb                    <- flatTimes r2
  , xyb                   <- splitSuffix xb yb
  , (x,y,b)               <- map3 Times xyb
  , b /= One               = facSuf $ Plus [x,y] <> b : rest
  
  -- x⋅b* + y⋅b*⋅b = (x + y⋅b)⋅b* -------------------------------
  | yb1                   <- flatTimes r2
  , (yb2, b1)             <- breakEnd isStar yb1
  , Just (y1, Star b2)    <- unsnoc yb2
  , b1 == flatTimes b2
  , yb                    <- y1 ++ b1 ++ [Star b2]
  , xb                    <- flatTimes r1
  , xyb                   <- splitSuffix xb yb
  , (x,y,b)               <- map3 Times xyb
  , b /= One               = facSuf $ Plus [x,y] <> b : rest
  
  ---------------------------------------------------------------
  | otherwise             = r1 : facSuf (r2 : rest)


flatTimes :: Regex -> [Regex]
flatTimes = \case
  Times xs -> xs
  x        -> [x]

isStar :: Regex -> Bool
isStar (Star _) = True
isStar _        = False

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


map3 :: (a -> b) -> (a,a,a) -> (b,b,b)
map3 f (x,y,z) = (f x, f y, f z)

