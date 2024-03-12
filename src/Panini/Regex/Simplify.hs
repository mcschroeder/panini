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
import Panini.Regex.Operations
import Panini.Regex.Type
import Prelude

-------------------------------------------------------------------------------

simplify :: Regex -> Regex
simplify = goFree
 where
  goFree r = case r of
    Plus  xs | r' <- factorChoices xs      , r' /= r -> goFree r'
             | r' <- liftChoices xs        , r' /= r -> goFree r'
             | r' <- lookupChoices xs      , r' /= r -> goFree r'
             | r' <- Plus (map goFree xs)  , r' /= r -> goFree r'
    Times xs | r' <- fuseSequence xs       , r' /= r -> goFree r'
             | r' <- liftSequence xs       , r' /= r -> goFree r'
             | r' <- pressSequence xs      , r' /= r -> goFree r'
             | r' <- lookupSequence xs     , r' /= r -> goFree r'
             | r' <- Times (map goFree xs) , r' /= r -> goFree r'
    Star  x  | r' <- Star (goStar x)       , r' /= r -> goFree r'    
    Opt   x  | r' <- Opt (goOpt x)         , r' /= r -> goFree r'
    _                                                -> r  
  
  goOpt r = case r of
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

flatNullable :: Regex -> Regex
flatNullable = \case
    Star r -> r
    Opt  r -> r
    r      -> r

-------------------------------------------------------------------------------

-- | Factorize an ordered list of choices by pairwise application of the
-- distributive laws, choosing the smallest overall outcome.
--
--     a⋅x + a⋅y = a⋅(x+y)            x⋅b + y⋅b = (x+y)⋅b
--
-- Factorization might peel off or re-arrange sub-expressions, if this leads to
-- a size reduction, according to the following additional laws:
--
--      a?⋅x + a ⋅y = a ⋅(   x + y) + x    x⋅b?    + y⋅b  = (x    + y)⋅b + x
--   a ⋅a*⋅x + a*⋅y = a*⋅(a ⋅x + y)        x⋅b*⋅b  + y⋅b* = (x⋅b  + y)⋅b*
--   a*⋅a ⋅x + a ⋅y = a ⋅(a*⋅x + y)        x⋅b ⋅b* + y⋅b  = (x⋅b* + y)⋅b
--
factorChoices :: [Regex] -> Regex
factorChoices xs = smaller
  (apply factorPrefixes xs)
  (apply (factorSuffixes . sortBy (compare `on` reverse)) xs)
 where
  apply f = Plus . map Times . f . map flatTimes

  factorPrefixes []         = []
  factorPrefixes [x]        = [x]
  factorPrefixes (ax:ay:zs) = case splitPrefix ax ay of    
    ---------------------------------------------------
    -- a?⋅x + a⋅y = x + a⋅(x+y)
    ([], Opt a1:x1, _)
      | (a,x,y) <- splitPrefix (flatTimes a1 ++ x1) ay
      , not (null a)
      , size (Times a) + 1 >= size (Times x)
      -> x : factorPrefixes (mkFacPre a x y : zs)

    ---------------------------------------------------
    -- a⋅x + a?⋅y = y + a⋅(x+y)
    ([], _, Opt a1:y1) 
      | (a,x,y) <- splitPrefix ax (flatTimes a1 ++ y1)
      , not (null a)
      , size (Times a) + 1 >= size (Times y)
      -> y : factorPrefixes (mkFacPre a x y : zs)    

    ---------------------------------------------------
    -- a*⋅a⋅x + a⋅y = a⋅(a*⋅x + y)
    ([], Star a1:ax1, _)
      | a1' <- flatTimes a1
      , (a2,x1) <- splitAt (length a1') ax1
      , a1' == a2
      , (a,x,y) <- splitPrefix (a2 ++ Star a1:x1) ay
      , not (null a)
      -> factorPrefixes (mkFacPre a x y : zs)
    
    ---------------------------------------------------
    -- a⋅x + a*⋅a⋅y = a⋅(x + a*⋅y)
    ([], _, Star a1:ay1)
      | a1' <- flatTimes a1
      , (a2,y1) <- splitAt (length a1') ay1
      , a1' == a2
      , (a,x,y) <- splitPrefix ax (a2 ++ Star a1:y1)
      , not (null a)
      -> factorPrefixes (mkFacPre a x y : zs)
    
    ---------------------------------------------------
    -- a⋅a*⋅x + a*⋅y = a*⋅(a⋅x + y)
    ([], break isStar -> (a1, Star a2:x1), _)
      | a1 == flatTimes a2
      , (a,x,y) <- splitPrefix (Star a2:a1 ++ x1) ay
      , not (null a)
      -> factorPrefixes (mkFacPre a x y : zs)
    
    ---------------------------------------------------
    -- a*⋅x + a⋅a*⋅y = a*⋅(x + a⋅y)
    ([], _, break isStar -> (a1, Star a2:y1))
      | a1 == flatTimes a2
      , (a,x,y) <- splitPrefix ax (Star a2:a1 ++ y1)
      , not (null a)
      -> factorPrefixes (mkFacPre a x y : zs)
    
    ---------------------------------------------------
    ([], _, _) -> ax : factorPrefixes (ay:zs)

    ---------------------------------------------------
    -- a⋅x + a⋅y = a⋅(x+y)
    (a, x, y) -> factorPrefixes (mkFacPre a x y : zs)

  mkFacPre a x y = a ++ [Plus [Times x, Times y]]

  factorSuffixes []         = []
  factorSuffixes [x]        = [x]
  factorSuffixes (xb:yb:zs) = case splitSuffix xb yb of
    ---------------------------------------------------
    -- x⋅b? + y⋅b = x + (x+y)⋅b
    (unsnoc -> Just (x1, Opt b1), _, []) 
      | (x,y,b) <- splitSuffix (x1 ++ flatTimes b1) yb
      , not (null b)
      , size (Times b) + 1 >= size (Times x)
      -> x : factorSuffixes (mkFacSuf x y b : zs)
    
    ---------------------------------------------------
    -- x⋅b + y⋅b? = y + (x+y)⋅b
    (_, unsnoc -> Just (y1, Opt b1), []) 
      | (x,y,b) <- splitSuffix xb (y1 ++ flatTimes b1)
      , not (null b)
      , size (Times b) + 1 >= size (Times y)
      -> y : factorSuffixes (mkFacSuf x y b : zs)
    
    ---------------------------------------------------
    -- x⋅b⋅b* + y⋅b = (x⋅b* + y)⋅b
    (unsnoc -> Just (xb1, Star b1), _, [])
      | b1' <- flatTimes b1
      , (x1,b2) <- splitAtEnd (length b1') xb1
      , b1' == b2
      , (x,y,b) <- splitSuffix (x1 ++ Star b1 : b2) yb
      , not (null b)
      -> factorSuffixes (mkFacSuf x y b : zs)
    
    ---------------------------------------------------
    -- x⋅b + y⋅b⋅b* = (x + y⋅b*)⋅b
    (_, unsnoc -> Just (yb1, Star b1), [])
      | b1' <- flatTimes b1
      , (y1,b2) <- splitAtEnd (length b1') yb1
      , b1' == b2
      , (x,y,b) <- splitSuffix xb (y1 ++ Star b1 : b2)
      , not (null b)
      -> factorSuffixes (mkFacSuf x y b : zs)
    
    ---------------------------------------------------
    -- x⋅b*⋅b + y⋅b* = (x⋅b + y)⋅b*
    ([], breakEnd isStar -> (unsnoc -> Just (x1, Star b2), b1), _)
      | b1 == flatTimes b2
      , (x,y,b) <- splitSuffix (x1 ++ b1 ++ [Star b2]) yb
      , not (null b)
      -> factorPrefixes (mkFacSuf x y b : zs)
    
    ---------------------------------------------------
    -- x⋅b* + y⋅b*⋅b = (x + y⋅b)⋅b*
    ([], _, breakEnd isStar -> (unsnoc -> Just (y1, Star b2), b1))
      | b1 == flatTimes b2
      , (x,y,b) <- splitSuffix xb (y1 ++ b1 ++ [Star b2])
      , not (null b)
      -> factorPrefixes (mkFacSuf x y b : zs)
    
    (_, _, []) -> xb : factorSuffixes (yb:zs)
    (x, y, b ) -> factorSuffixes (mkFacSuf x y b : zs)

  mkFacSuf x y b = Plus [Times x, Times y] : b

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

-- | Apply known syntactic replacements of sequences.
--
--     (1)  x* ⋅ y ⋅ (x* ⋅ y)* = (x + y)* ⋅ y
--
lookupSequence :: [Regex] -> Regex
lookupSequence = Times . go
 where
  go (Star x1 : y1 : Star (Times [Star x2, y2]) : zs)  -- (1)
    | x1 == x2, y1 == y2 
    = go $ Star (Plus [x1,y1]) <> y1 : zs
  
  go (y:ys) = y : go ys
  go [] = []

-- | Apply known syntactic replacements of choices.
-- 
--     (1)  x⋅Σ* + x* = (x⋅Σ*)?
--
lookupChoices :: [Regex] -> Regex
lookupChoices = (Plus .) $ go $ \case
  (Times [x1, All], Star x2) | x1 == x2 -> Just $ Opt (Times [x1, All])  -- (1)
  _ -> Nothing 
 where
  go _ []     = []
  go f (x:xs) = go1 xs []
   where
    go1 []     zs                      = x : go f zs
    go1 (y:ys) zs | Just x' <- f (x,y) = go f (x' : ys ++ zs)
    go1 (y:ys) zs                      = go1 ys (y:zs)

-------------------------------------------------------------------------------

-- | Choose the smaller of two regular expressions in terms of 'size'.
smaller :: Regex -> Regex -> Regex
smaller r1 r2 
  | size r1 <= size r2 = r1
  | otherwise          = r2

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

