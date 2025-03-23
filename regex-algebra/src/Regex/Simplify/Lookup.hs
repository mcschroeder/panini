module Regex.Simplify.Lookup where

import Data.List qualified as List
import Data.List (partition)
import Prelude hiding (lookup)
import Regex.CharSet qualified as CS
import Regex.Simplify.Common
import Regex.Simplify.Factor (splitPrefix, splitSuffix, flatTimes)
import Regex.Type

-- TODO: read syntactic replacements from a file

lookup :: Context -> Regex -> Regex

lookup Starred = \case

  -- (a⋅((c⋅a)*⋅b + c + …))*  =  (a⋅(b+c+…))*
  TimesN a p@(Plus xs)
    | let as = flatTimes a
    , Just xs' <- deleteFindMatch [] as xs    
    -> a `times` Plus xs'
   where
    deleteFindMatch _ _ [] = Nothing
    deleteFindMatch ys as ((Times1 (Star (Times ca)) b) : zs)
      | (cs,[],_) <- splitSuffix ca as
      , let c = Times cs
      , c `isChoiceOf` p
      = Just (b : ys ++ zs)
    deleteFindMatch ys as (z:zs) = deleteFindMatch (z:ys) as zs

  -- (ab(a?b + …)* + …)*  =  (ab(b + …)* + …)*
  TimesN ab (Star (Plus xs))
    | let xs' = map match xs
    , xs' /= xs
    -> ab <> Star (Plus xs')
   where
    match = \case
      Times1 (Opt a2) b2
        | let as = flatTimes a2
        , (a1, [], b1) <- splitPrefix as (flatTimes ab)
        , a1 == as
        , b1 == flatTimes b2
        -> b2
      x -> x
  
  -- ((a⋅(b⋅a*)*)*⋅c)*  =  ((a⋅b*)*⋅c)*
  Times1 (Star (TimesN a1 (Star (TimesN b (Star a2))))) c
    | a1 == a2
    -> Star (Star (a1 <> Star b) <> c)

  -- ([ab] + c⋅((a⋅b*)*⋅c)*⋅a)*  =  (b + c*a)*
  Plus [Lit ab, Times [c1, Star (Times1 (Star (TimesN (Lit a1) (Star (Lit b)))) c2), Lit a2]]
    | a1 == a2, c1 == c2
    , ab == CS.union a1 b
    -> Star (Lit b `plus` ((Star c1) <> Lit a1))
  
  r -> lookup Optional r

lookup Optional = lookup Free

lookup _ = \case

  -- (x + y(z + .*a)).*  =  (x + yz).*
  -- (x + y(.*a)?).*     =  (x + y).*
  -- (y(z + .*a))?.*     =  (yz)?.*
  -- (y(.*a)?)?.*        =  y?.*
  Times1 r All
    | Plus (partition is_yza -> ([get_yz -> Just yz], xs')) <- r -> Plus (yz:xs') <> All
    | Opt yza <- r, is_yza yza, Just yz <- get_yz yza -> (Opt yz) <> All
   where
    is_yza (TimesN _ (Plus (partition is_a -> ([_], _)))) = True
    is_yza (TimesN _ (Opt (is_a -> True)))                = True
    is_yza _                                              = False

    get_yz (TimesN y (Plus (partition is_a -> ([_], zs)))) = Just (y <> (Plus zs))
    get_yz (TimesN y (Opt (is_a -> True)))                 = Just y
    get_yz _                                               = Nothing

    is_a (Times1 All (Lit _)) = True
    is_a _                    = False


  Times xs0 -> Times $ go xs0
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

    -- (x⋅y*⋅z)*⋅x⋅y*  =  x⋅(y + z⋅x)*
    go (Star (Times xyz) : xy_rest)
      | Just (x1, y1, z) <- splitAtStar xyz
      , Just (x2, y2, rest) <- splitAtStar xy_rest
      , x1 == x2, y1 == y2
      , let zx = Times (z ++ x1)
      = go $ x1 ++ Star (Plus [y1, zx]) : rest

    -- a ⋅ ((b*a)*a)* ⋅ b ⋅ b* ⋅ (a(b(b*a)?)*)?  =  a(a+b)*ba?
    go (a1 : Star x : b2 : Star b3 : Opt y : xs)
      | Times1 (Star (Times1 (Star b1) a2)) a3 <- x
      , Times1 a4 (Star (Times1 b4 (Opt (Times1 (Star b5) a5)))) <- y
      , a1 == a2, a2 == a3, a3 == a4, a4 == a5
      , b1 == b2, b2 == b3, b3 == b4, b4 == b5
      = go $ a1 : Star (a1 `plus` b1) : b1 : Opt a1 : xs

    -- bb*(a(a*ba?)*)?  =  b(a*b)*a?
    go (b1 : Star b2 : Opt (Times1 a1 (Star (Times [Star a2, b3, Opt a3]))) : xs)
      | a1 == a2, a2 == a3, b1 == b2, b2 == b3
      = go $ b1 : Star (Star a1 `times` b1) : Opt a1 : xs

    -- b*(b+a(a*ba?)*)  =  a+(a+b)*ba?
    go (Star b1 : Plus1 b2 (Times1 a1 (Star (Times [Star a2, b3, Opt a3]))) : xs)
      | a1 == a2, a2 == a3
      , b1 == b2, b2 == b3
      = go $ (a1 `plus` (Star (a1 `plus` b1) `times` b1 `times` Opt a1)) : xs

    -- (a?ā)*a?aa.*  =  (a?ā)*aa.*
    go (r@(Star (Times1 (Opt a1) ā1)) : Opt a2 : a3 : a4 : All : xs)
      | a1 == a2, a2 == a3, a3 == a4
      , Lit a <- a1
      , Lit ā <- ā1, CS.complement a == ā
      = go $ r : a3 : a4 : All : xs

    -- b*(a(b(b*a)?)*)?  =  (a?b)*a?
    go (Star b1 : Opt (Times1 a1 (Star (Times1 b2 (Opt (Times1 (Star b3) a2))))) : xs)
      | a1 == a2
      , b1 == b2, b2 == b3
      = go $ Star (Opt a1 <> b1) : Opt a1 : xs
    
    -- [^a]*(a([^ab]b*)*)*  =  b*(a|[^ab]b*)*
    go (Star (Lit ā) : Star (Times1 (Lit a) (Star r@(Times1 (Lit nab) (Star (Lit b))))) : xs)
      | ā == CS.complement a
      , nab == CS.complement (CS.union a b)
      = go $ Star (Lit b) : Star (Lit a `plus` r) : xs

    go (y:ys) = y : go ys
    go [] = []
  
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

  Plus xs0 -> Plus $ go g xs0
   where
    g = \case
      -- .*[ab].* + [^a]*  =  .*
      (Times [All, Lit ab, All], Star (Lit ā))
        | let a = CS.complement ā
        , CS.size ab == 2, a `CS.isSubsetOf` ab
        -> Just All

      -- [^a]*a.* + [^a]*  =  .*
      (Times [Star (Lit ā), Lit a, All], Star (Lit ā1))
        | ā == ā1, ā == CS.complement a
        -> Just All

      -- a*b(b+aa*b)*(c+aa*c) + a*c  =  (a+b)*c
      (Times [Star a1, b1, Star (Plus [b2, Times [a2, Star a3, b3]]), Plus [c1, Times [a4, Star a5, c2]]], Times1 (Star a6) c3)
        | a1 == a2, a2 == a3, a3 == a4, a4 == a5, a5 == a6
        , b1 == b2, b2 == b3
        , c1 == c2, c2 == c3
        -> Just $ Star (a1 `plus` b1) `times` c1

      -- x* + x⋅Σ*  =  (x⋅Σ*)?
      (Star x1, Times [x2, All])
        | x1 == x2 
        -> Just $ Opt (Times [x1, All])
        
      -- b* + b*a(āb*a)*(āb*)?  =  (b+aā)*(ab*)?
      (Star b4, Times [Star b1, a1, Star (Times [ā1, Star b2, a2]), Opt (Times [ā2, Star b3])])
        | b1 == b2, b2 == b3, b3 == b4, a1 == a2, ā1 == ā2
        , Lit a  <- a1
        , Lit a' <- ā1, a' == CS.complement a
        -> Just $ Star (Plus [b1, a1 <> ā1]) <> Opt (a1 <> Star b1)

      -- b + a(ba)*bb  =  (ab)*b
      (b, Times [a, Star (Times [b2,a2]), b3, b4])
        | b == b2, b2 == b3, b3 == b4, a == a2
        -> Just $ Star (a <> b) <> b

      -- ā⋅Σ*⋅a + a⋅(ā*⋅a)*  =  Σ*⋅a
      (Times [Lit ā1, All, Lit a1], Times [Lit a2, Star (Times [Star (Lit ā2), Lit a3])])
        | a1 == a2, a2 == a3, ā1 == ā2, ā1 == CS.complement a1
        -> Just $ All <> Lit a1
      
      -- x* + (x*⋅y⋅x*)*  =  (x + y)*
      (Star x, Star (Times [Star x1, y, Star x2]))
        | x == x1, x1 == x2
        -> Just $ Star (x `plus` y)

      -- .*a|a*  =  (ā*a)*
      (Times1 All a1, Star a2)
        | a1 == a2 
        , Lit a <- a1
        , let ā = Lit (CS.complement a)
        -> Just $ Star (Star ā <> a1)
      
      -- (a*b)*b + b*  =  ((a*b)*b)*
      (Times1 (Star (Times1 (Star a) b1)) b2, Star b3)
        | b1 == b2, b2 == b3
        -> Just $ Star (Star (Star a <> b1) <> b1)

      _ -> Nothing
    
    go _ []     = []
    go f (x:xs) = go1 xs []
     where
      go1 []     zs                      = x : go f (reverse zs)
      go1 (y:ys) zs | Just x' <- f (x,y) = go f (x' : ys ++ zs)
      go1 (y:ys) zs                      = go1 ys (y:zs)

  -- (x + x?y)?  =  x?y?
  Opt (Plus [x1, Times [Opt x2, y]])
    | x1 == x2 
    -> Opt x1 <> Opt y
  
  -- (y + xy?)?  =  x?y?
  Opt (Plus [y1, Times (unsnoc -> Just (x, Opt y2))])
    | y1 == y2 
    -> Opt (Times x) <> Opt y1
  
  -- (x(y*z+y*))?  =  (xy*z?)?
  Opt (Times [x, Plus [Times (Star y1 : z), Star y2]])
    | y1 == y2 
    -> Opt (x <> Star y1 <> Opt (Times z))

  -- (b(ā + ab)*a?)?  =  (bΣ*a?)*
  Opt (Times [Lit b, Star (Plus [Lit ā, Times [Lit a, Lit b2]]), Opt (Lit a2)])
    | b == b2, a == a2, ā == CS.complement a
    -> Star (Lit b <> All <> Opt (Lit a))

  -- ((x+y)⋅x*)?  =  y?⋅x*
  Opt (Times [Plus [z1,z2], Star x])
    | z1 == x -> Opt z2 <> Star x
    | z2 == x -> Opt z1 <> Star x

  -- (ā(ā((ā)*(ā + aΣ*))? + aΣ*)? + a(b(bΣ*)?)?)?  =  (a(b(b(b̄)*)*)? + (āa*)*)
  Opt (Plus [Times [ā1, Opt (Plus [Times [ā2, Opt (Times [Star ā3, (Plus [ā4, Times [a1, All]])])], Times [a2, All]])], Times [a3, Opt (Times [b1, Opt (Times [b2, All])])]])
    | ā1 == ā2, ā2 == ā3, ā3 == ā4, a1 == a2, a2 == a3, b1 == b2
    , Lit a <- a1
    , Lit b <- b1
    , Lit a' <- ā1, a' == CS.complement a
    , let b̄1 = Lit (CS.complement b)
    -> Plus [Times [a1, Opt (Times [b1, Star (Times [b1, Star b̄1])])], Star (Times [ā1, Star a1])]

  -- ({a,b}+ab)?  =  a?b?
  Opt (Plus [Lit ab, Times [Lit a, Lit b]])
    | ab == a `CS.union` b
    -> Times [Opt (Lit a), Opt (Lit b)]

  -- ({a,b,c}+a(c+bc?)+bc)?  =  a?b?c
  Opt (Plus [Lit abc, Times [Lit a1, (Plus [Lit c1, Times [Lit b1, Opt (Lit c2)]])], Times [Lit b2, Lit c3]])
    | b1 == b2, c1 == c2, c2 == c3
    , abc == a1 `CS.union` b1 `CS.union` c1
    -> Times [Opt (Lit a1), Opt (Lit b1), Opt (Lit c1)]

  -- ({a,b,c}+a(b+b?c)+bc)?  =  a?b?c
  Opt (Plus [Lit abc, Times [Lit a1, (Plus [Lit b1, Times [Opt (Lit b2), Lit c1]])], Times [Lit b3, Lit c2]])
    | b1 == b2, b2 == b3, c1 == c2
    , abc == a1 `CS.union` b1 `CS.union` c1
    -> Times [Opt (Lit a1), Opt (Lit b1), Opt (Lit c1)]

  -- ((.*a)?a)?  =  ((ā*a)*a)?
  Opt (Times1 (Opt (Times1 All a1)) a2)
    | a1 == a2
    , Lit a <- a1
    , let ā = Lit (CS.complement a)
    -> Opt (Star (Star ā <> a1) <> a1)

  -- (x + y⋅x?)*  = (x + y)*
  Star (Plus1 x1 (TimesN y (Opt x2)))
    | x1 == x2
    -> Star (x1 `plus` y)

  -- (x + (y ⋅ (x + y)*))*  =  (x + y)*
  Star (Plus [x1, Times (unsnoc -> Just (y1, Star (Plus [x2, Times y2])))])
    | x1 == x2, y1 == y2 -> Star (Plus [x1, Times y1])
  
  -- (x + (y ⋅ x*))*  =  (x + y)*
  Star (Plus [x1, Times (unsnoc -> Just (y1, Star x2))])
    | x1 == x2 -> Star (Plus [x1, Times y1])

  -- (x + y ⋅ (x* + y)*)*  =  (x + y)*
  Star (Plus [x1, Times (unsnoc -> Just (y1, Star (Times (Star x2 : y2))))])
    | x1 == x2, y1 == y2 -> Star (Plus [x1, Times y1])
  
  -- (x + x*y)*  =  (x+y)*
  Star (Plus [x1, Times (Star x2 : y)])
    | x1 == x2 -> Star (Plus [x1, Times y])

  -- (x + y⋅(z⋅x*⋅y)*⋅z)*  =  (x + y⋅z)*
  Star (Plus [x1, Times yzxyz])
    | Just (y1, Times zxy, z2) <- splitAtStar yzxyz
    , Just (z1, x2, y2) <- splitAtStar zxy
    , x1 == x2, y1 == y2, z1 == z2
    -> Star (Plus [x1, Times y1 <> Times z1])

  -- (x⋅(x*⋅y)? + y⋅(y + x)*)*  =  (x + y)*
  Star (Plus [Times xxy, Times yyx])
    | Just (x1, Opt xy) <- unsnoc xxy
    , Times (Star x2 : y1) <- xy
    , Just (y2, Star yx) <- unsnoc yyx
    , Plus [y3, x3] <- yx
    , x <- Times x1
    , y <- Times y1
    , x == x2, x2 == x3
    , y1 == y2, y == y3
    -> Star (Plus [x,y])

  -- (x + y⋅(x + y)*)*  =  (x + y)*
  Star (Plus [x, Times yxy])
    | Just (y1, Star xy) <- unsnoc yxy
    , Plus [z1,z2] <-xy
    , let y = Times y1
    , (x == z1 && y == z2) || (x == z2 && y == z1)
    -> Star (Plus [x,y])
  
  -- ((x⋅(y*⋅x)*)?⋅y)*  =  (x*⋅y)*
  Star (Times [Opt (Times [x1, Star (Times [Star y1, x2])]), y2])
    | x1 == x2, y1 == y2
    -> Star (Times [Star x1, y1])

  -- (a⋅(ā⋅Σ*⋅a)?)*  =  (a⋅(ā*⋅a)*)*
  Star (Times [Lit a1, Opt (Times [Lit ā, All, Lit a2])])
    | a1 == a2, ā == CS.complement a1
    -> Star (Times [Lit a1, Star (Times [Star (Lit ā), Lit a1])])

  -- (x+(y+(x+yz)*)z)*  =  (x+y?z)*
  Star (Plus1 x1 (TimesN (Plus1 y1 (Star (Plus1 x2 yz))) z2))
    | x1 == x2
    , let ys = flatTimes y1
    , (y2, [], z1) <- splitPrefix ys (flatTimes yz)
    , y2 == ys
    , z1 == flatTimes z2
    -> Star (x1 `plus` (Opt y1 `times` z2))

  -- (x+yz+z(x+y?z)*)*  =  (x+y?z)*
  Star (Plus [x1, yz, TimesN z2 (Star (Plus1 x2 w@(Times1 (Opt y2) z3)))])
    | x1 == x2, z2 == z3
    , let ys = flatTimes y2
    , (y1, [], z1) <- splitPrefix ys (flatTimes yz)
    , y1 == ys
    , z1 == flatTimes z2
    -> Star (x1 `plus` w)

  -- ((bb*a)*a)*  =  ((b*a)*a)*
  Star (Times1 (Star (Times [b1, Star b2, a1])) a2)
    | b1 == b2, a1 == a2
    -> Star ((Star ((Star b1) `times` a1)) `times` a1)
  
  -- (a(a+b)*ba?+b(b*a)?)*  =  (a*ba?)*
  -- (a{a,b}*ba?+b(b*a)?)*  =  (a*ba?)*
  Star (Plus1 x y)
    | Times1 b1 (Opt (Times1 (Star b2) a1)) <- y
    , Times [a2, Star z, b3, Opt a3] <- x
    , a1 == a2, a2 == a3
    , b1 == b2, b2 == b3
    , z == Plus [a1,b1]
    -> Star (Star a1 `times` b1 `times` Opt a1)

  -- (b(a(a*ba?)*)?)*  =  (b(a*b)*a?)*
  Star (Times1 b1 (Opt (Times1 a1 (Star (Times [Star a2, b2, Opt a3])))))
    | a1 == a2, a2 == a3, b1 == b2
    -> Star (b1 `times` Star (Star a1 `times` b1) `times` Opt a1)

  -- (x + x?y((x?y)*z)+(x+y)*)*  =  (x+yz?)*
  Star (Plus1 x (Times [Opt x2, y, Plus1 (Times1 (Star q) z) (Star p)]))
    | p == Plus [x,y]
    , q == Times [Opt x, y]
    , x == x2
    -> Star (x `plus` (y `times` Opt z))
  
  -- (x⋅(y* + …) + y + …)*  = (x⋅(…)? + y + …)*
  Star r@(Plus xs0)
    | Just xs' <- go [] xs0
    -> Star (Plus xs')
   where
    go zs (TimesN x (Plus ys) : xs)
      | (y:_) <- [y | Star y <- ys, y `isChoiceOf` r]
      , let ys' = List.delete (Star y) ys
      = Just $ (x `times` Opt (Plus ys')) : zs ++ xs
    go zs (x:xs) = go (x:zs) xs
    go _ [] = Nothing
  
  -- (ā(ā*a)? + aΣ*āa?)*  =  (a*āa?)*
  Star (Plus [Times [b1, Opt (Times1 (Star b2) a1)], Times [a2, All, b3, Opt a3]])
    | b1 == b2, b2 == b3, a1 == a2, a2 == a3
    , Lit a <- a1, Lit b <- b1
    , a == CS.complement b
    -> Star (Star a1 `times` b1 `times` Opt a1)
  
  r -> r

splitAtStar :: [Regex] -> Maybe ([Regex], Regex, [Regex])
splitAtStar = go []
 where
  go _            []  = Nothing
  go ys (Star x : xs) = Just (reverse ys, x, xs)
  go ys (     x : xs) = go (x:ys) xs

isChoiceOf :: Regex -> Regex -> Bool
isChoiceOf (Lit a) (Lit b) = CS.isSubsetOf a b
isChoiceOf (Lit a) (Plus1 (Lit b) _) = CS.isSubsetOf a b
isChoiceOf x (Plus ys) = x `elem` ys
isChoiceOf _ _ = False

------------------------------------------------------------------------------

-- | Look up known regex complements.
lookupComplement :: Regex -> Maybe Regex
lookupComplement = \case
  -- ¬(.*ab.*)  =  b*(a|[^ab]b*)*
  Times [All, a@(Lit a0), b@(Lit b0), All]
    | CS.intersection a0 b0 == CS.empty
    , let c = Lit (CS.complement (CS.union a0 b0))
    -> Just $ Star b <> Star (a `plus` (c <> Star b))
  
  -- ¬(.*a[^b].*) = ([^ab]|a?b)*a?
  Times [All, a@(Lit a0), Lit b̄0, All]
    | let b0 = CS.complement b̄0
    , CS.intersection a0 b0 == CS.empty
    , let b = Lit b0
    , let c = Lit (CS.complement (CS.union a0 b0))
    -> Just $ Star (c `plus` (Opt a <> b)) <> Opt a
  
  -- ¬(.*aa.*)  =  (a?[^a])*a?
  Times [All, a@(Lit a0), a2, All]
    | a == a2
    , let ā = Lit (CS.complement a0)
    -> Just $ Star (Opt a <> ā) <> Opt a

  -- ¬(.*abb.*) = b*(ab?|[^ab]b*)*
  Times [All, a@(Lit a0), b@(Lit b0), b2, All]
    | b == b2
    , CS.intersection a0 b0 == CS.empty
    , let c = Lit (CS.complement (CS.union a0 b0))
    -> Just $ Star b <> Star ((a <> Opt b) `plus` (c <> Star b))
  
  -- ¬(b*(a|[^ab]b*)*)  =  .*ab.*
  Times1 (Star b) (Star (Plus1 a (Times1 c (Star b2))))
    | b == b2    
    , Lit a0 <- a
    , Lit b0 <- b
    , CS.intersection a0 b0 == CS.empty
    , Lit c0 <- c, c0 == CS.complement (CS.union a0 b0)    
    -> Just $ All <> a <> b <> All

  -- ¬((a?[^a])*a?)  =  .*aa.*
  Times1 (Star (Times1 (Opt a) (Lit ā))) (Opt a2)
    | a == a2
    , Lit a0 <- a
    , ā == CS.complement a0
    -> Just $ All <> a <> a <> All

  -- ¬(b*(ab?|[^ab]b*)*) = .*abb.*
  Times1 (Star b) (Star (Plus1 (Times1 a (Opt b2)) (Times1 c (Star b3))))
    | b == b2, b2 == b3
    , Lit a0 <- a
    , Lit b0 <- b
    , CS.intersection a0 b0 == CS.empty
    , Lit c0 <- c, c0 == CS.complement (CS.union a0 b0)
    -> Just $ All <> a <> b <> b <> All
  
  -- ¬(b*(ab?|[^ab]b*)*abb.*) = b*(ab?|[^ab]b*)*
  Times [Star b, r@(Star (Plus1 (Times1 c (Star b3)) (Times1 a (Opt b2)))), a2, b4, b5, All]
    | a == a2
    , b == b2, b2 == b3, b3 == b4, b4 == b5
    , Lit a0 <- a
    , Lit b0 <- b
    , CS.intersection a0 b0 == CS.empty
    , Lit c0 <- c, c0 == CS.complement (CS.union a0 b0)
    -> Just $ Star b <> r

  -- ¬(b*(ab?|[^ab]b*)*abb) = bb?|(b*([^b]|([^b]|([^a]|abb.*a)b)b))*
  Times [Star b, Star (Plus1 (Times1 c (Star b3)) (Times1 a (Opt b2))), a2, b4, b5]
    | a == a2
    , b == b2, b2 == b3, b3 == b4, b4 == b5
    , Lit a0 <- a
    , Lit b0 <- b
    , CS.intersection a0 b0 == CS.empty
    , Lit c0 <- c, c0 == CS.complement (CS.union a0 b0)
    , let ā = Lit (CS.complement a0)
    , let b̄ = Lit (CS.complement b0)
    -> Just $ (b <> Opt b) `plus` Star (Star b <> (b̄ `plus` ((b̄ `plus` ((ā `plus` (a <> b <> b <> All <> a)) <> b)) <> b)))

  -- ¬((a?[^a])*a?aa)  =  .*aa.*aa|(a*[^a])*a?
  Times [Star (Times1 (Opt a) (Lit ā)), Opt a2, a3, a4]
    | a == a2, a2 == a3, a3 == a4
    , Lit a0 <- a
    , ā == CS.complement a0
    -> Just $ (All <> a <> a <> All <> a <> a) `plus` (Star (Star a <> Lit ā) <> Opt a)
    
  -- ¬((a?[^a])*a?aa.*)  =  (a?[^a])*a?
  Times [Star (Times1 (Opt a) (Lit ā)), Opt a2, a3, a4, All]
    | a == a2, a2 == a3, a3 == a4
    , Lit a0 <- a
    , ā == CS.complement a0
    -> Just $ Star (Opt a <> Lit ā) <> Opt a

  -- ¬(Σ*a)  =  (Σ*(Σ∖a))?
  Times [All, Lit a] 
    -> Just $ Opt (All <> Lit (CS.complement a))
  
  -- ¬(.*a.?)  =  ((.*[^a])?[^a])?
  Times [All, Lit a0, Opt AnyChar]
    | let ā = Lit (CS.complement a0)
    -> Just $ Opt (Opt (All <> ā) <> ā)
  
  -- ¬(((.*[^a])?[^a])?)  =  .*a.?
  Opt (Times1 (Opt (Times1 All (Lit ā0))) (Lit ā1))
    | ā0 == ā1
    , let a = Lit (CS.complement ā0)
    -> Just $ All <> a <> Opt AnyChar

  _ -> Nothing
