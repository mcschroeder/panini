module Panini.Regex.Simplify.Lookup where

import Panini.Regex.CharSet qualified as CS
import Panini.Regex.Simplify.Common
import Panini.Regex.Type
import Prelude

-- TODO: read syntactic replacements from a file

lookup :: Context -> Regex -> Regex
lookup _ = \case  
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
      -- x⋅Σ* + x*  =  (x⋅Σ*)?
      (Times [x1, All], Star x2) 
        | x1 == x2 
        -> Just $ Opt (Times [x1, All])

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
    
    go _ []     = []
    go f (x:xs) = go1 xs []
     where
      go1 []     zs                      = x : go f zs
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

  -- ({a,b,c}+a(c+bc?)+bc)?  =  a?b?c
  Opt (Plus [Lit abc, Times [Lit a1, (Plus [Lit c1, Times [Lit b1, Opt (Lit c2)]])], Times [Lit b2, Lit c3]])
    | b1 == b2, c1 == c2, c2 == c3
    , abc == a1 `CS.union` b1 `CS.union` c1
    -> Times [Opt (Lit a1), Opt (Lit b1), Opt (Lit c1)]

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

  r -> r

splitAtStar :: [Regex] -> Maybe ([Regex], Regex, [Regex])
splitAtStar = go []
 where
  go _            []  = Nothing
  go ys (Star x : xs) = Just (reverse ys, x, xs)
  go ys (     x : xs) = go (x:ys) xs
