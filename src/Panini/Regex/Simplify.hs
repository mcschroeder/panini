{-
This module implements regular expression simplifications based on the
transformations from Kahrs and Runcimcan (2022).

References:

  * Kahrs, Stefan and Colin Runciman. 2022. "Simplifying Regular Expressions
    Further." Journal of Symbolic Computation 109 (2022): 124–143.
    https://doi.org/10.1016/j.jsc.2021.08.003

-}
module Panini.Regex.Simplify (simplify) where

import Data.Function
import Data.List.Extra (partition, sortBy, uncons, splitAtEnd, breakEnd)
import Panini.Regex.CharSet (CharSet)
import Panini.Regex.CharSet qualified as CS
import Panini.Regex.Equivalence
import Panini.Regex.Operations
import Panini.Regex.Type
import Prelude

-------------------------------------------------------------------------------

simplify :: Regex -> Regex
simplify = goFree
 where
  goFree r = case r of
    _        | r' <- lookupRegex r         , r' /= r -> r'
    Plus  xs | r' <- lookupChoices xs      , r' /= r -> goFree r'
             | r' <- factorPrefixes xs     , r' /= r -> goFree r'
             | r' <- factorSuffixes xs     , r' /= r -> goFree r'
             | r' <- liftChoices xs        , r' /= r -> goFree r'             
             | r' <- subsumeChoices xs     , r' /= r -> goFree r'
             | r' <- Plus (map goFree xs)  , r' /= r -> goFree r'
    Times xs | r' <- lookupSequence xs     , r' /= r -> goFree r'
             | r' <- fuseSequence xs       , r' /= r -> goFree r'
             | r' <- liftSequence xs       , r' /= r -> goFree r'
             | r' <- pressSequence xs      , r' /= r -> goFree r'             
             | r' <- Times (map goFree xs) , r' /= r -> goFree r'
    Star  x  | r' <- lookupStar x          , r' /= r -> goFree r'
             | r' <- Star (goStar x)       , r' /= r -> goFree r'    
    Opt   x  | r' <- lookupOpt x           , r' /= r -> goFree r' 
             | r' <- Opt (goOpt x)         , r' /= r -> goFree r'
    _                                                -> r  
  
  goOpt r = case r of
    Plus xs  | r' <- fuseOptChoices xs     , r' /= r -> goOpt r'
    Times xs | r' <- fuseOptSequence xs    , r' /= r -> goOpt r'             
    x                                                -> goFree x
  
  goStar r = case r of
    Plus  xs | r' <- fuseStarChoices xs    , r' /= r -> goStar r'
             | r' <- liftStarChoices xs    , r' /= r -> goStar r'
    Times xs | r' <- fuseStarSequence xs   , r' /= r -> goStar r'
    Lit   x                                          -> Lit x
    _        | r' <- liftStar r            , r' /= r -> goStar r'
    _                                                -> goOpt r

-------------------------------------------------------------------------------

-- | Apply the free fusion law on a sequence of regular expressions.
--
--    x* ⋅ x* = x* ⋅ x? = x? ⋅ x* = c*
--
fuseSequence :: [Regex] -> Regex
fuseSequence = Times . go
 where
  go (Star x1 : Star x2 : ys) | x1 == x2 = go (Star x1 : ys)
  go (Star x1 : Opt  x2 : ys) | x1 == x2 = go (Star x1 : ys)
  go (Opt  x1 : Star x2 : ys) | x1 == x2 = go (Star x1 : ys)
  go (y:ys) = y : go ys
  go [] = []

-- | Apply the starred fusion law on a sequence of regular expressions.
--
--    (x? ⋅ y?)* = (x + y)*
--
fuseStarSequence :: [Regex] -> Regex
fuseStarSequence xs
  | all nullable xs = Plus $ map flatNullable xs
  | otherwise       = Times xs

-- | Apply the starred fusion law on a list of choices.
--
--    (x + y?)* = (x + y*)* = (x + y)*
--
fuseStarChoices :: [Regex] -> Regex
fuseStarChoices = Plus . map flatNullable

flatNullable :: Regex -> Regex
flatNullable = \case
    Star r -> r
    Opt  r -> r
    r      -> r

-- | Apply fusion rules on an optional sequence of expression.
--
--    (xx*)? = (x*)?
--    (x*x)? = (x*)?
--
fuseOptSequence :: [Regex] -> Regex
fuseOptSequence = \case
  [x1, Star x2] | x1 == x2 -> Star x2
  [Star x1, x2] | x1 == x2 -> Star x1
  xs -> Times xs

-- | Apply fusion rules within an optional list of choices.
--
--    (x + yy*)? = (x + y*)?
--    (x + y*y)? = (x + y*)?
--
fuseOptChoices :: [Regex] -> Regex
fuseOptChoices xs = Plus $ map go xs
 where
  go (Times ys) = fuseOptSequence ys
  go y          = y

-------------------------------------------------------------------------------

-- | Factorize an ordered list of choices by pairwise application of the
-- left-distributive law and its special cases.
factorPrefixes :: [Regex] -> Regex
factorPrefixes xs = Plus $ go xs
 where
  go (x:y:zs) 
    | z <- factorPrefix      x y, size z < size (Plus [x,y]) =     go (z:zs)
    | z <- factorPrefixOpt   x y, size z < size (Plus [x,y]) =     go (z:zs)
    | z <- factorPrefixStar1 x y, size z < size (Plus [x,y]) =     go (z:zs)
    | z <- factorPrefixStar2 x y, size z < size (Plus [x,y]) =     go (z:zs)
    | otherwise                                              = x : go (y:zs)
  go [x]                                                     = [x]
  go []                                                      = []

-- | Factorize two choices using the left-distributive law.
--
--    a⋅x + a⋅y = a⋅(x+y)
--
factorPrefix :: Regex -> Regex -> Regex
factorPrefix r1 r2
  -----------------------------------------------------------------------
  | ax      <- flatTimes r1
  , ay      <- flatTimes r2
  , axy     <- splitPrefix ax ay
  , (a,x,y) <- map3 Times axy
  , a /= One                                  = a <> Plus [x,y]
  -----------------------------------------------------------------------
  | otherwise                                 = Plus [r1,r2]
  -----------------------------------------------------------------------

-- | Factorize two choices using a special case of the left-distributive law.
--
--    (a⋅b)?⋅x + a⋅y = x + a⋅(b⋅x + y)
--    a⋅x + (a⋅b)?⋅y = y + a⋅(x + b⋅y)
--
factorPrefixOpt :: Regex -> Regex -> Regex
factorPrefixOpt r1 r2
  -----------------------------------------------------------------------
  | Opt ab : x1 <- flatTimes r1
  , abx         <- flatTimes ab ++ x1
  , ay          <- flatTimes r2  
  , (a,bx,y)    <- map3 Times $ splitPrefix abx ay
  , x           <- Times x1
  , a /= One
  , bx /= One
  , size a + 1 >= size x                     = Plus [x, a <> Plus [bx,y]]
  -----------------------------------------------------------------------
  | Opt ab : y1 <- flatTimes r2
  , aby         <- flatTimes ab ++ y1
  , ax          <- flatTimes r1
  , (a,x,by)    <- map3 Times $ splitPrefix ax aby
  , y           <- Times y1
  , a /= One
  , by /= One
  , size a + 1 >= size y                     = Plus [y, a <> Plus [x,by]]
  -----------------------------------------------------------------------
  | otherwise                                = Plus [r1,r2]
  -----------------------------------------------------------------------

-- | Factorize two choices using a special case of the left-distributive law.
--
--    a*⋅a⋅x + a⋅y = a⋅(a*⋅x + y)
--    a⋅x + a*⋅a⋅y = a⋅(x + a*⋅y)
--
factorPrefixStar1 :: Regex -> Regex -> Regex
factorPrefixStar1 r1 r2
  -----------------------------------------------------------------------
  | Star a1 : ax1 <- flatTimes r1
  , a2            <- flatTimes a1
  , (a3,x1)       <- splitAt (length a2) ax1
  , a2 == a3
  , ax            <- a3 ++ Star a1 : x1
  , ay            <- flatTimes r2
  , axy           <- splitPrefix ax ay
  , (a,x,y)       <- map3 Times axy
  , a /= One                                  = a <> Plus [x,y]
  -----------------------------------------------------------------------
  | Star a1 : ay1 <- flatTimes r2
  , a2            <- flatTimes a1
  , (a3,y1)       <- splitAt (length a2) ay1
  , a2 == a3
  , ay            <- a3 ++ Star a1 : y1
  , ax            <- flatTimes r1
  , axy           <- splitPrefix ax ay
  , (a,x,y)       <- map3 Times axy
  , a /= One                                  = a <> Plus [x,y]
  -----------------------------------------------------------------------
  | otherwise                                 = Plus [r1,r2]
  -----------------------------------------------------------------------

-- | Factorize two choices using a special case of the left-distributive law.
--
--    a⋅a*⋅x + a*⋅y = a*⋅(a⋅x + y)
--    a*⋅x + a⋅a*⋅y = a*⋅(x + a⋅y)
--
factorPrefixStar2 :: Regex -> Regex -> Regex
factorPrefixStar2 r1 r2
  -----------------------------------------------------------------------
  | ax1                <- flatTimes r1
  , (a1, Star a2 : x1) <- break isStar ax1
  , a1 == flatTimes a2
  , ax                 <- Star a2 : a1 ++ x1
  , ay                 <- flatTimes r2
  , axy                <- splitPrefix ax ay
  , (a,x,y)            <- map3 Times axy
  , a /= One                                  = a <> Plus [x,y]
  -----------------------------------------------------------------------
  | ay1                <- flatTimes r2
  , (a1, Star a2 : y1) <- break isStar ay1
  , a1 == flatTimes a2  
  , ay                 <- Star a2 : a1 ++ y1
  , ax                 <- flatTimes r2
  , axy                <- splitPrefix ax ay
  , (a,x,y)            <- map3 Times axy
  , a /= One                                  = a <> Plus [x,y]
  -----------------------------------------------------------------------
  | otherwise                                 = Plus [r1,r2]
  -----------------------------------------------------------------------

-------------------------------------------------------------------------------

-- | Factorize an ordered list of choices by pairwise application of the
-- right-distributive law and its special cases.
factorSuffixes :: [Regex] -> Regex
factorSuffixes xs = Plus $ go $ sortBy (compare `on` (reverse . flatTimes)) xs
 where
  go (x:y:zs)
    | z <- factorSuffix      x y, size z < size (Plus [x,y]) =     go (z:zs)
    | z <- factorSuffixOpt   x y, size z < size (Plus [x,y]) =     go (z:zs)
    | z <- factorSuffixStar1 x y, size z < size (Plus [x,y]) =     go (z:zs)
    | z <- factorSuffixStar2 x y, size z < size (Plus [x,y]) =     go (z:zs)
    | otherwise                                              = x : go (y:zs)
  go [x]                                                     = [x]
  go []                                                      = []

-- | Factorize two choices using the right-distributive law.
--
--    x⋅b + y⋅b = (x+y)⋅b
--
factorSuffix :: Regex -> Regex -> Regex
factorSuffix r1 r2
  -----------------------------------------------------------------------
  | xb      <- flatTimes r1
  , yb      <- flatTimes r2
  , xyb     <- splitSuffix xb yb
  , (x,y,b) <- map3 Times xyb
  , b /= One                                  = Plus [x,y] <> b
  -----------------------------------------------------------------------
  | otherwise                                 = Plus [r1,r2]
  -----------------------------------------------------------------------

-- | Factorize two choices using a special case of the right-distributive law.
--
--    x⋅(a⋅b)? + y⋅b = x + (x⋅a + y)⋅b
--    x⋅b + y⋅(a⋅b)? = y + (x + y⋅a)⋅b
--
factorSuffixOpt :: Regex -> Regex -> Regex
factorSuffixOpt r1 r2
  -----------------------------------------------------------------------
  | Just (x1, Opt ab) <- unsnoc $ flatTimes r1
  , xab               <- x1 ++ flatTimes ab
  , yb                <- flatTimes r2
  , xayb              <- splitSuffix xab yb
  , (xa,y,b)          <- map3 Times xayb
  , x                 <- Times x1
  , b /= One
  , xa /= One  
  , size b + 1 >= size x                     = Plus [x, Plus [xa,y] <> b]
  -----------------------------------------------------------------------
  | Just (y1, Opt ab) <- unsnoc $ flatTimes r2
  , yab               <- y1 ++ flatTimes ab
  , xb                <- flatTimes r1
  , xyab              <- splitSuffix xb yab
  , (x,ya,b)          <- map3 Times xyab
  , y                 <- Times y1
  , b /= One
  , ya /= One
  , size b + 1 >= size y                     = Plus [y, Plus [x,ya] <> b]
  -----------------------------------------------------------------------
  | otherwise                                = Plus [r1,r2]
  -----------------------------------------------------------------------

-- | Factorize two choices using a special case of the right-distributive law.
--
--    x⋅b⋅b* + y⋅b = (x⋅b* + y)⋅b
--    x⋅b + y⋅b⋅b* = (x + y⋅b*)⋅b
--
factorSuffixStar1 :: Regex -> Regex -> Regex
factorSuffixStar1 r1 r2
  -----------------------------------------------------------------------
  | xb1                 <- flatTimes r1
  , Just (xb2, Star b1) <- unsnoc xb1
  , b2                  <- flatTimes b1
  , (x1,b3)             <- splitAtEnd (length b2) xb2
  , b2 == b3
  , xb                  <- x1 ++ Star b1 : b3
  , yb                  <- flatTimes r2
  , xyb                 <- splitSuffix xb yb
  , (x,y,b)             <- map3 Times xyb
  , b /= One                                  = Plus [x,y] <> b
  -----------------------------------------------------------------------
  | yb1                 <- flatTimes r2
  , Just (yb2, Star b1) <- unsnoc yb1
  , b2                  <- flatTimes b1
  , (y1,b3)             <- splitAtEnd (length b2) yb2
  , b2 == b3
  , yb                  <- y1 ++ Star b1 : b3
  , xb                  <- flatTimes r1
  , xyb                 <- splitSuffix xb yb
  , (x,y,b)             <- map3 Times xyb
  , b /= One                                  = Plus [x,y] <> b
  -----------------------------------------------------------------------
  | otherwise                                 = Plus [r1,r2]
  -----------------------------------------------------------------------


-- | Factorize two choices using a special case of the right-distributive law.
--
--    x⋅b*⋅b + y⋅b* = (x⋅b + y)⋅b*
--    x⋅b* + y⋅b*⋅b = (x + y⋅b)⋅b*
--
factorSuffixStar2 :: Regex -> Regex -> Regex
factorSuffixStar2 r1 r2
  -----------------------------------------------------------------------
  | xb1                 <- flatTimes r1
  , (xb2, b1)           <- breakEnd isStar xb1
  , Just (x1, Star b2)  <- unsnoc xb2
  , b1 == flatTimes b2
  , xb                  <- x1 ++ b1 ++ [Star b2]
  , yb                  <- flatTimes r2
  , xyb                 <- splitSuffix xb yb
  , (x,y,b)             <- map3 Times xyb
  , b /= One                                  = Plus [x,y] <> b
  -----------------------------------------------------------------------
  | yb1                 <- flatTimes r2
  , (yb2, b1)           <- breakEnd isStar yb1
  , Just (y1, Star b2)  <- unsnoc yb2
  , b1 == flatTimes b2
  , yb                  <- y1 ++ b1 ++ [Star b2]
  , xb                  <- flatTimes r1
  , xyb                 <- splitSuffix xb yb
  , (x,y,b)             <- map3 Times xyb
  , b /= One                                  = Plus [x,y] <> b
  -----------------------------------------------------------------------
  | otherwise                                 = Plus [r1,r2]

-------------------------------------------------------------------------------

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

-- TODO: replace by Data.List.unsnoc once we depend on base-4.19
unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs = (\(hd, tl) -> (reverse tl, hd)) <$> uncons (reverse xs)

map3 :: (a -> b) -> (a,a,a) -> (b,b,b)
map3 f (x,y,z) = (f x, f y, f z)

-------------------------------------------------------------------------------

-- | Apply the basic lifting law to a starred expression.
--
--    x* = α̂₁(x)*  if α(x) = α₁(x)
--
liftStar :: Regex -> Regex
liftStar r | alpha r == a1 = Lit a1 where a1 = alpha1 r
liftStar r                 = r

-- | Apply the generalized lifting law on a starred list of choices.
--
--     (x + y)* = (α̂₁(x) + y)*  if α(x) ⊆ α₁(x + y)
--
liftStarChoices :: [Regex] -> Regex
liftStarChoices xs = case partition (\x -> alpha x `CS.isSubsetOf` a1) xs of
  ([], _ ) -> Plus xs
  (ys, zs) -> Plus $ map (Lit . alpha1) ys ++ zs
 where
  a1 = alpha1 (Plus xs)

-- | Apply lifting to adjacent subsequences.
--
--    x* ⋅ y? = x*  if α(y) ⊆ α₁(x)
--
liftSequence :: [Regex] -> Regex
liftSequence = Times . go
 where
  go (Star x : Star y : zs) | alpha y `CS.isSubsetOf` alpha1 x = go (Star x : zs)
  go (Star x : Opt  y : zs) | alpha y `CS.isSubsetOf` alpha1 x = go (Star x : zs)
  go (x:xs) = x : go xs
  go [] = []


-- | Apply lifting to subsume choices.
--
--    x* + y = x*  if α(y) ⊆ α₁(x)
  --
liftChoices :: [Regex] -> Regex
liftChoices = Plus . go
 where
  go (x:xs) = go1 x xs []
  go [] = []

  go1 (Star x) (y:ys) zs | alpha y `CS.isSubsetOf` alpha1 x = go1 (Star x) ys zs
  go1 x (Star y : ys) zs | alpha x `CS.isSubsetOf` alpha1 y = go (Star y : ys ++ zs)
  go1 x (y:ys) zs = go1 x ys (y:zs)
  go1 x [] zs = x : go zs

-------------------------------------------------------------------------------

-- | Replace self-star-equivalent nullable subsequences with starred choices.
--
--     x?⋅y? = (x + y)*  if L(x?⋅y?) = L ((x?⋅y?)*)
---
pressSequence :: [Regex] -> Regex
pressSequence = Times . go []
 where
  go ys (x:xs)
    | nullable x = go (x:ys) xs
    | otherwise  = tryPress (reverse ys) ++ x : go [] xs
  go ys []       = tryPress (reverse ys)

  tryPress []               = []
  tryPress [y]              = [y]
  tryPress ys
    | selfStarEq (Times ys) = [Star (Plus $ map flatNullable ys)]
    | otherwise             = ys

-- | Checks whether L(r) = L(r*).
selfStarEq :: Regex -> Bool
selfStarEq r = equivalence r (Star r)
-- TODO: can we somehow exploit this special case of equivalence checking?

-------------------------------------------------------------------------------

-- TODO: read syntactic replacements from a file

-- | Apply known syntactic replacements of regexes.
lookupRegex :: Regex -> Regex
lookupRegex = \case
  -- (ā(ā((ā)*(ā + aΣ*))? + aΣ*)? + a(b(bΣ*)?)?)?  =  (a(b(b(b̄)*)*)? + (āa*)*)
  Opt (Plus [Times [ā1, Opt (Plus [Times [ā2, Opt (Times [Star ā3, (Plus [ā4, Times [a1, All]])])], Times [a2, All]])], Times [a3, Opt (Times [b1, Opt (Times [b2, All])])]])
    | ā1 == ā2, ā2 == ā3, ā3 == ā4, a1 == a2, a2 == a3, b1 == b2
    , Lit a <- a1
    , Lit b <- b1
    , Lit a' <- ā1, a' == CS.complement a
    , let b̄1 = Lit (CS.complement b)
    -> Plus [Times [a1, Opt (Times [b1, Star (Times [b1, Star b̄1])])], Star (Times [ā1, Star a1])]
  
  -- (āΣ* + (ā)*(a(ā(a(b̄ + a*b) + b(b̄)*)*a* + (a(b(a(b̄ + a*b) + b(b̄)*)*)*)*))?)  =  ((ā)*(a(ā(a(b̄ + a*b) + b(b̄)*)*a* + (a(b(a(b̄ + a*b) + b(b̄)*)*)*)*))? + (āa*)*)
  Plus [Times [ā1, All], Times [Star ā2, Opt (Times [a1, (Plus [Times [ā3, Star (Plus [Times [a2, (Plus [b̄1, Times [Star a3, b1]])], Times [b2, Star b̄2]]), Star a4], Star (Times [a5, Star (Times [b3, Star (Plus [Times [a6, Plus [b̄3, Times [Star a7, b4]]], Times [b5, Star b̄4]])])])])])]]
    | all (ā1 ==) [ā2,ā3]
    , all (a1 ==) [a2,a3,a4,a5,a6,a7]
    , all (b̄1 ==) [b̄2,b̄3,b̄4]
    , all (b1 ==) [b2,b3,b4,b5]
    , Lit a <- a1
    , Lit b <- b1
    , Lit a' <- ā1, a' == CS.complement a
    , Lit b' <- b̄1, b' == CS.complement b
    , let x = Plus [Times [a1, Plus [b̄1, Times [Star a1, b1]]], Times [b1, Star b̄1]]
    -> Plus [Times [Star ā1, Opt (Times [a1, (Plus [Times [ā1, Star x, Star a1], Star (Times [a1, Star (Times [b1, Star x])])])])], Star (Times [ā1, Star a1])]

  r -> r

-- | Apply known syntactic replacements of sequences.
lookupSequence :: [Regex] -> Regex
lookupSequence = Times . go
 where
  -- x*⋅y⋅(x*⋅y)*  =  (x + y)* ⋅ y
  go (Star x1 : y1 : Star (Times [Star x2, y2]) : zs)
    | x1 == x2, y1 == y2 
    = go $ Star (Plus [x1,y1]) <> y1 : zs
  
  -- a(bc*a)*bc*  =  abc*(abc*)*
  go (a : Star (Times [b, Star c, a2]) : b2 : Star c2 : zs)
    | a == a2, b == b2, c == c2
    = go $ a <> b <> Star c <> Star (a <> b <> Star c) : zs
  
  -- a(bc*a)*b  =  ab(c*ab)*
  go (a : Star (Times [b, Star c, a2]) : b2 : zs)
    | a == a2, b == b2
    = go $ a <> b <> Star (Star c <> a <> b) : zs

  -- (y*x)*y*  =  (x+y)*
  go (Star (Times (Star y : x)) : Star y2 : zs)
    | y == y2
    = go $ Star (Plus [Times x, y]) : zs

  -- b̄*⋅(a(b̄ + a*b) + bb̄* + a*b)* = Σ*
  go (Star b̄1 : Star (Plus [Times [a1, Plus [b̄2, Times [Star a2, b1]]], Times [b2, Star b̄3], Times [Star a3, b3]]) : zs)
    | b̄1 == b̄2, b̄2 == b̄3, a1 == a2, a2 == a3, b1 == b2, b2 == b3
    , Lit _ <- a1
    , Lit b <- b1
    , Lit b' <- b̄1, b' == CS.complement b
    = go $ All : zs

  go (y:ys) = y : go ys
  go [] = []

-- | Apply known syntactic replacements of choices.
lookupChoices :: [Regex] -> Regex
lookupChoices = (Plus .) $ go $ \case
  -- x⋅Σ* + x*  =  (x⋅Σ*)?
  (Times [x1, All], Star x2) | x1 == x2 -> Just $ Opt (Times [x1, All])

  -- [ab] + ab  =  a?b?
  (Lit cs, Times [Lit a, Lit b]) 
    | cs == CS.union a b -> Just $ Opt (Lit a) <> Opt (Lit b)

  -- (b*a(āb*a)*(āb*)? + b*)  =  (b+aā)*(ab*)?
  (Times [Star b1, a1, Star (Times [ā1, Star b2, a2]), Opt (Times [ā2, Star b3])], Star b4)
    | b1 == b2, b2 == b3, b3 == b4, a1 == a2, ā1 == ā2
    , Lit a  <- a1
    , Lit a' <- ā1, a' == CS.complement a
    -> Just $ Star (Plus [b1, a1 <> ā1]) <> Opt (a1 <> Star b1)

  -- b + a(ba)*bb  =  (ab)*b
  (b, Times [a, Star (Times [b2,a2]), b3, b4])
    | b == b2, b2 == b3, b3 == b4, a == a2
    -> Just $ Star (a <> b) <> b

  _ -> Nothing
 where
  go _ []     = []
  go f (x:xs) = go1 xs []
   where
    go1 []     zs                      = x : go f zs
    go1 (y:ys) zs | Just x' <- f (x,y) = go f (x' : ys ++ zs)
    go1 (y:ys) zs                      = go1 ys (y:zs)

-- | Apply known syntactic replacements of optionals.
lookupOpt :: Regex -> Regex
lookupOpt = \case
  -- (x + x?y)?  =  x?y?
  Plus [x1, Times [Opt x2, y]]
    | x1 == x2 
    -> Opt x1 <> Opt y
  
  -- (y + xy?)?  =  x?y?
  Plus [y1, Times (unsnoc -> Just (x, Opt y2))]
    | y1 == y2 
    -> Opt (Times x) <> Opt y1
  
  -- (x(y*z+y*))?  =  (xy*z?)?
  Times [x, Plus [Times (Star y1 : z), Star y2]]
    | y1 == y2 
    -> Opt (x <> Star y1 <> Opt (Times z))

  -- (b(ā + ab)*a?)?  =  (bΣ*a?)*
  Times [Lit b, Star (Plus [Lit ā, Times [Lit a, Lit b2]]), Opt (Lit a2)]
    | b == b2, a == a2, ā == CS.complement a
    -> Star (Lit b <> All <> Opt (Lit a))

  x                                       -> Opt x

-- | Apply known syntactic replacements of starred expressions.
lookupStar :: Regex -> Regex
lookupStar = \case
  -- (x + (y ⋅ (x + y)*))*  =  (x + y)*
  Plus [x1, Times (unsnoc -> Just (y1, Star (Plus [x2, Times y2])))]
    | x1 == x2, y1 == y2 -> Star (Plus [x1, Times y1])
  
  -- (x + (y ⋅ x*))*  =  (x + y)*
  Plus [x1, Times (unsnoc -> Just (y1, Star x2))]
    | x1 == x2 -> Star (Plus [x1, Times y1])

  -- (x + y ⋅ (x* + y)*)*  =  (x + y)*
  Plus [x1, Times (unsnoc -> Just (y1, Star (Times (Star x2 : y2))))]
    | x1 == x2, y1 == y2 -> Star (Plus [x1, Times y1])
  
  -- (x + x*y)*  =  (x+y)*
  Plus [x1, Times (Star x2 : y)]
    | x1 == x2 -> Star (Plus [x1, Times y])

  x -> Star x

-------------------------------------------------------------------------------

subsumeChoices :: [Regex] -> Regex
subsumeChoices = go []
 where
  go ys []                        = Plus ys
  go ys (x:xs)
    | any (subsumes x) (xs ++ ys) = go ys xs
    | otherwise                   = go (x:ys) xs
  
  subsumes x y = equivalence x (intersection x y)

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

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

