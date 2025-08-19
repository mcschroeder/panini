{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
module Panini.Abstract.Semantics
  ( normRelA
  , normExprA
  , abstract
  , concretizeUnit
  , concretizeBool
  , concretizeInt
  , concretizeChar
  , concretizeString
  , isDeferredStrComp
  , pattern AStringComp
  ) where

import Algebra.Lattice
import Data.Generics.Uniplate.Operations as Uniplate
import Data.Maybe
import Panini.Abstract.ABool as ABool
import Panini.Abstract.AChar as AChar
import Panini.Abstract.AInt as AInt
import Panini.Abstract.AString as AString
import Panini.Abstract.AUnit as AUnit
import Panini.Abstract.AValue
import Panini.Abstract.Interval (pattern (:…))
import Panini.Panic
import Panini.Pretty
import Panini.Provenance
import Panini.Syntax hiding (Value(..))
import Panini.Syntax.QQ
import Prelude
import Regex qualified as Regex
import Regex.POSIX.ERE qualified
import Regex.Type (prettyRegex)

--import Debug.Trace qualified
trace :: String -> a -> a
trace _ = id
--trace = Debug.Trace.trace

-- TODO: built-in tracing via Pan monad (plus non-monadic versions of each function)

-------------------------------------------------------------------------------
-- In this module, we use some special notation to simplify pattern matching.

(.<) :: AInt -> Integer -> Bool
(.<) = AInt.isLe

pattern Relℤ :: AExpr -> ARel -> AExpr
pattern Relℤ x r <- (matchERelA TInt -> Just (x,r))

matchERelA :: Base -> AExpr -> Maybe (AExpr, ARel)
matchERelA b (ERelA v b1 r) | b == b1 = Just (EVar v b, r)
matchERelA _ _ = Nothing

-------------------------------------------------------------------------------

-- | Normalize an abstract expression by (partial) evaluation; see 'normRel'.
normExprA :: AExpr -> AExpr
normExprA = rewrite $ \case
  -----------------------------------------------------------------------------
  ERelA x _ (isolate x -> EVar x1 _ :=: y) | x == x1, x `notFreeIn` y -> Just y
  -----------------------------------------------------------------------------
  ERelA x b r -> case normRelA r of
    Left True            -> Just $ EAbs (topValue b)
    Left False           -> Just $ EAbs (botValue b)
    Right r' | r' /= r   -> Just $ ERelA x b r'
             | otherwise -> case abstract x b r of
                              ARel _ _ _ -> Nothing
                              a          -> Just $ EAbs a
  -----------------------------------------------------------------------------
  [ω| not(p)      |] -> Just $ EBoolA (neg p)
  [ω| not(not(x)) |] -> Just x
  -----------------------------------------------------------------------------
  [ω| n + _ |] | isBot n -> Just $ EIntA bot
  [ω| n - _ |] | isBot n -> Just $ EIntA bot
  [ω| _ + n |] | isBot n -> Just $ EIntA bot
  [ω| _ - n |] | isBot n -> Just $ EIntA bot
  [ω| m + n |]           -> Just $ EIntA (AInt.add m n)
  [ω| m - n |]           -> Just $ EIntA (AInt.sub m n)
  [ω| x + 0 |]           -> Just x
  [ω| x - 0 |]           -> Just x
  [ω| x + n |] | n .< 0  -> Just [ω| x - n' |] where n' = AInt.negate n
  [ω| x - n |] | n .< 0  -> Just [ω| x + n' |] where n' = AInt.negate n
  -----------------------------------------------------------------------------
  [ω| (x + k) + n |] -> Just [ω| x + (k + n) |]
  [ω| (x + k) - n |] -> Just [ω| x + (k - n) |]
  [ω| (k + x) + n |] -> Just [ω| x + (k + n) |]
  [ω| (k + x) - n |] -> Just [ω| x + (k - n) |]
  [ω| (x - k) + n |] -> Just [ω| x - (k - n) |]
  [ω| (x - k) - n |] -> Just [ω| x - (k + n) |]
  [ω| (k - x) + n |] -> Just [ω| (k + n) - x |]
  [ω| (k - x) - n |] -> Just [ω| (k - n) - x |]
  -----------------------------------------------------------------------------
  [ω| mod(m̂,n̂) |] | [m] <- m̂, [n] <- n̂ -> Just $ EIntA [m `mod` n]
  -----------------------------------------------------------------------------  
  [ω| |s| |]
    | isTop s             -> Just $ EIntA (AIntFrom 0)
    | Just n <- strLen1 s -> Just $ EIntA [n]
  -- NOTE: We don't have any efficient way to compute nor represent, in general,
  -- the precise lengths of all strings contained in an abstract string.
  -----------------------------------------------------------------------------
  [ω| ŝ[i] |] | AString1 s <- ŝ -> Just $ ECharA (charAt s i)
  [ω| s[î] |] | [i] <- î        -> Just $ ECharA (charsAt s i)
  [ω| x[str.len(x)] |] -> Just $ ECharA bot
  -----------------------------------------------------------------------------
  [ω| x[|x|+n] |]
    | let n' = n ∧ AIntTo (-1)
    , n' /= n
    -> Just [ω| x[|x|+n'] |]
  -----------------------------------------------------------------------------
  [ω| ŝ[i..j]     |] | AString1 s <- ŝ -> Just $ EStrA (strSub s i j)
  [ω| s[î..î]     |] | [i] <- î        -> Just $ EStrA (lit $ charsAt s i)
  [ω| x[0..|x|-1] |]                   -> Just x
  -----------------------------------------------------------------------------
  [ω| x[î..ĵ][m̂..n̂] |]
    | [i] <- î, [j] <- ĵ, [m] <- m̂, [n] <- n̂
    , i >= 0, i <= j, m >= 0, m <= n, n - m <= j - i
    -> Just [ω| x[î+m̂..î+m̂+(n̂-m̂)] |]
  -----------------------------------------------------------------------------
  [ω| re.comp(re.comp(x)) |] -> Just x
  -- NOTE: We want to defer resolution of EStrComp as long as possible,
  -- in order to exploit opportunities for double-negation cancellation!
  -----------------------------------------------------------------------------
  [ω| s ++ t |] -> Just $ EStrA  (s ⋅ t)
  -----------------------------------------------------------------------------
  [ω| x[î..ĵ] ++ x[m̂..n̂] |]
    | [i] <- î, [j] <- ĵ, [m] <- m̂, [n] <- n̂
    , i <= j, j + 1 == m, m <= n
    -> Just [ω| x[î..n̂] |]
  -----------------------------------------------------------------------------
  EStrStar (EStrA s) -> Just $ EStrA (star s)
  -----------------------------------------------------------------------------
  [ω| str.contains(s,t) |] -> Just $ EBoolA [t ⊑ s]
  -----------------------------------------------------------------------------
  [ω| |x[0..str.indexof(x,s,0)-1]| |] -> Just [ω| str.indexof(x,s,0) |]
  -----------------------------------------------------------------------------
  _ -> Nothing

-- | Normalize an abstract relation by (partial) evaluation; see 'normRel'.
normRelA :: ARel -> Either Bool ARel
normRelA r0 = trace ("normRelA " ++ showPretty r0 ++ " --> " ++ either show showPretty r1) r1 
 where
 r1 = case r0 of
--normRelA = \case
  -----------------------------------------------------------------------------
  -- after this, all subexpressions are fully normalized
  r | r' <- descendBi normExprA r, r' /= r -> normRelA r'
  -----------------------------------------------------------------------------
  [ρ| x ≠ n |] -> normRelA [ρ| x = n' |] where n' = neg n
  [ρ| x < n |] -> normRelA [ρ| x = n' |] where n' = AInt.ltA n
  [ρ| x ≤ n |] -> normRelA [ρ| x = n' |] where n' = AInt.leA n
  [ρ| x > n |] -> normRelA [ρ| x = n' |] where n' = AInt.gtA n
  [ρ| x ≥ n |] -> normRelA [ρ| x = n' |] where n' = AInt.geA n
  -----------------------------------------------------------------------------
  [ρ| x < y |] -> normRelA [ρ| x = y - n |] where n = AInt.ge 1
  [ρ| x ≤ y |] -> normRelA [ρ| x = y - n |] where n = AInt.ge 0
  [ρ| x > y |] -> normRelA [ρ| x = y + n |] where n = AInt.ge 1
  [ρ| x ≥ y |] -> normRelA [ρ| x = y + n |] where n = AInt.ge 0
  -----------------------------------------------------------------------------
  -- NOTE: ">" is the structural ordering on 'AExpr'; after 
  -- this block, the "smaller" expression will be on the LHS,
  -- with variables < functions < constants
  x :=: y | x > y -> normRelA $ y :=: x
  x :≠: y | x > y -> normRelA $ y :≠: x
  -----------------------------------------------------------------------------
  EAbs a :=: EAbs b | Just c <- a ∧? b -> Left (not $ hasBot c)
  EAbs a :≠: EAbs b | Just c <- a ∧? b -> Left (hasBot c)
  -----------------------------------------------------------------------------
  x :∈: EReg r -> normRelA $ x :=: EStrA (AString.fromRegex $ Regex.POSIX.ERE.toRegex r)
  x :∉: EReg r -> normRelA $ x :≠: EStrA (AString.fromRegex $ Regex.POSIX.ERE.toRegex r)
  -----------------------------------------------------------------------------
  [ρ| ¬x = true  |] -> normRelA [ρ| x = false |]
  [ρ| ¬x = false |] -> normRelA [ρ| x = true  |]
  -----------------------------------------------------------------------------
  x :=: _ | anyBot x -> Left False
  _ :=: x | anyBot x -> Left False
  x :≠: _ | anyBot x -> Left True
  _ :≠: x | anyBot x -> Left True
  -----------------------------------------------------------------------------
  [ρ| x = x     |] -> Left True
  [ρ| x = x + n |] -> Left (AInt.member 0 n)
  [ρ| x = x - n |] -> Left (AInt.member 0 n)
  [ρ| x + n = x |] -> Left (AInt.member 0 n)
  [ρ| x - n = x |] -> Left (AInt.member 0 n)
  -----------------------------------------------------------------------------
  [ρ| x ≠ x     |] -> Left False
  [ρ| x ≠ x + n |] -> Left (not $ AInt.member 0 n)
  [ρ| x ≠ x - n |] -> Left (not $ AInt.member 0 n)
  [ρ| x + n ≠ x |] -> Left (not $ AInt.member 0 n)
  [ρ| x - n ≠ x |] -> Left (not $ AInt.member 0 n)
  -----------------------------------------------------------------------------
  Rel o [ω| k + x |] [ω| n     |] -> normRelA $ Rel o x [ω| n - k       |]
  Rel o [ω| x + k |] [ω| n     |] -> normRelA $ Rel o x [ω| n - k       |]
  Rel o [ω| k + x |] [ω| n + y |] -> normRelA $ Rel o x [ω| y + (n - k) |]
  Rel o [ω| x + k |] [ω| n + y |] -> normRelA $ Rel o x [ω| y + (n - k) |]
  Rel o [ω| k + x |] [ω| y + n |] -> normRelA $ Rel o x [ω| y + (n - k) |]
  Rel o [ω| x + k |] [ω| y + n |] -> normRelA $ Rel o x [ω| y + (n - k) |]
  Rel o [ω| k + x |] [ω| n - y |] -> normRelA $ Rel o x [ω| (n - k) - y |]
  Rel o [ω| x + k |] [ω| n - y |] -> normRelA $ Rel o x [ω| (n - k) - y |]
  Rel o [ω| k + x |] [ω| y - n |] -> normRelA $ Rel o x [ω| y - (k + n) |]
  Rel o [ω| x + k |] [ω| y - n |] -> normRelA $ Rel o x [ω| y - (k + n) |]
  Rel o [ω| k - x |] [ω| n     |] -> normRelA $ Rel o x [ω| k - n       |]
  Rel o [ω| x - k |] [ω| n     |] -> normRelA $ Rel o x [ω| k + n       |]
  Rel o [ω| k - x |] [ω| n + y |] -> normRelA $ Rel o x [ω| (k - n) - y |]
  Rel o [ω| x - k |] [ω| n + y |] -> normRelA $ Rel o x [ω| y + (k + n) |]
  Rel o [ω| k - x |] [ω| y + n |] -> normRelA $ Rel o x [ω| (k - n) - y |]
  Rel o [ω| x - k |] [ω| y + n |] -> normRelA $ Rel o x [ω| y + (k + n) |]
  Rel o [ω| k - x |] [ω| n - y |] -> normRelA $ Rel o x [ω| y + (k - n) |]
  Rel o [ω| x - k |] [ω| n - y |] -> normRelA $ Rel o x [ω| (k + n) - y |]
  Rel o [ω| k - x |] [ω| y - n |] -> normRelA $ Rel o x [ω| (k + n) - y |]
  Rel o [ω| x - k |] [ω| y - n |] -> normRelA $ Rel o x [ω| y + (k - n) |]
  -----------------------------------------------------------------------------
  [ρ| str.indexof(x,y,z) = n |] 
    | let n' = n ∧ AIntFrom (-1)
    , n' /= n
    -> normRelA $ [ρ| str.indexof(x,y,z) = n' |]
  -----------------------------------------------------------------------------
  [ρ| str.indexof(x,c,0) = n |] 
    -> normRelA $ x :=: EStrA (strWithIndexOfChar c n AInt0)
  -----------------------------------------------------------------------------
  [ρ| str.indexof(x,s,î) = n |]
    | AIntFrom 0 <- n
    , [i] <- î
    -> normRelA $ x :=: EStrA (rep Σ i ⋅ star Σ ⋅ s ⋅ star Σ)
  -----------------------------------------------------------------------------
  [ρ| str.indexof(x,y,î) = n |]
    | [i] <- î
    , let n' = (n ∧ AIntFrom i) ∨ (if AInt.member (-1) n then [-1] else [])
    , n' /= n
    -> normRelA [ρ| str.indexof(x,y,î) = n' |]
  -----------------------------------------------------------------------------
  [ρ| x[str.indexof(x,s,î)+ĵ..|x|-1] = t |]
    | [j] <- ĵ
    , Just n <- strLen1 s
    , n == j
    , [i] <- î
    -> normRelA $ x :=: EStrA (rep Σ i ⋅ star Σ ⋅ s ⋅ t)    
  -----------------------------------------------------------------------------
  [ρ| str.indexof(x,y,|x|+n) = z |]
    | [Fin i :… PosInf] <- AInt.intervals n
    , i < 0
    , let n' = AInt.fromTo 0 (Prelude.negate i)
    -> normRelA $ [ρ| str.indexof(x,y,|x|-n') = z |]
  -----------------------------------------------------------------------------
  [ρ| str.indexof(x,s,n) = -1 |]
    | AIntFrom i <- n
    , let s̄ = neg (star Σ ⋅ s ⋅ star Σ)
    -> normRelA $ x :=: EStrA (rep Σ i ⋅ star Σ ⋅ s̄)
    | [i] <- n
    , let s̄ = neg (star Σ ⋅ s ⋅ star Σ)
    -> normRelA $ x :=: EStrA (rep Σ i ⋅ s̄)
  -----------------------------------------------------------------------------
  [ρ| str.indexof(x,c,|x|-1) = -1 |] 
    -> normRelA $ x :=: EStrA (star Σ ⋅ lit (neg c))
  -----------------------------------------------------------------------------
  [ρ| str.indexof(x,c,|x|-n) = -1 |]
    | AIntFrom i <- n
    , let c̄ = lit (neg c)
    -> normRelA $ x :=: EStrA (star Σ ⋅ rep c̄ i ⋅ star c̄)
  -----------------------------------------------------------------------------
  [ρ| str.indexof(x,c,|x|-n) = -1 |]
    | [0,1] <- n
    , let c̄ = lit (neg c)
    -> normRelA $ x :=: EStrA (star Σ ⋅ opt c̄)
  -----------------------------------------------------------------------------
  [ρ| str.indexof(x,y,z) = |x| |] 
    -> normRelA [ρ| str.indexof(x,y,z) = -1 |] 
  -----------------------------------------------------------------------------
  [ρ| z = str.indexof(x,y,z) |] -> normRelA [ρ| x[z..z+|y|-1] = y |]
  -----------------------------------------------------------------------------
  [ρ| z ≠ str.indexof(x,c,z) |]
    | let c̄ = neg c
    -> normRelA [ρ| z = str.indexof(x,c̄,0) |]
  -----------------------------------------------------------------------------
  [ρ| z + n = str.indexof(x,y,z) |]
    | let n' = n ∧ AIntFrom 0
    , n' /= n
    -> normRelA [ρ| z + n' = str.indexof(x,y,z) |]
  -----------------------------------------------------------------------------
  [ρ| z - n = str.indexof(x,y,z) |]
    | let n' = n ∧ AIntTo 0
    , n' /= n
    -> normRelA [ρ| z - n' = str.indexof(x,y,z) |]
  -----------------------------------------------------------------------------
  [ρ| |x| + n = str.indexof(x,y,z) |]
    | let n' = n ∧ AIntTo (-1)
    , n' /= n
    -> normRelA [ρ| |x| + n' = str.indexof(x,y,z) |]
  -----------------------------------------------------------------------------
  [ρ| |x| - n = str.indexof(x,y,z) |]
    | let n' = n ∧ AIntFrom 1
    , n' /= n
    -> normRelA [ρ| |x| - n' = str.indexof(x,y,z) |]
  -----------------------------------------------------------------------------
  [ρ| |x| - 1 = str.indexof(x,c,î) |]
    | [i] <- î
    -> normRelA $ x :=: EStrA (rep Σ i ⋅ star (lit (neg c)) ⋅ lit c)
  -----------------------------------------------------------------------------
  [ρ| |x| - n = str.indexof(x,c,î) |]
    | AIntFrom 0 <- n
    , [i] <- î
    -> normRelA $ x :=: EStrA (rep Σ i ⋅ star Σ ⋅ opt (lit c ⋅ star Σ))
  -----------------------------------------------------------------------------
  [ρ| |x| - n = str.indexof(x,c,î) |]
    | AIntFrom j <- n, j >= 1
    , [i] <- î
    -> normRelA $ x :=: EStrA (rep Σ i ⋅ star Σ ⋅ lit c ⋅ rep Σ (j-1) ⋅ star Σ)
  -----------------------------------------------------------------------------
  [ρ| |x| - ĵ = str.indexof(x,s,î) |]
    | Just n <- strLen1 s
    , [j] <- ĵ ∧ AIntFrom n
    , [i] <- î
    -> normRelA $ x :=: EStrA (rep Σ i ⋅ neg (star Σ ⋅ s ⋅ star Σ) ⋅ s ⋅ rep Σ (j - n))
  -----------------------------------------------------------------------------
  [ρ| |x| - n = str.indexof(x,_,|x|-1) |]
    | AIntFrom j <- n, j > 1
    -> Left False
  -----------------------------------------------------------------------------
  [ρ| str.indexof(x,c,str.indexof(x,c̄,0)) = -1 |] 
    | c̄ == neg c
    -> normRelA $ x :=: EStrA (star (lit c) ⋅ star (lit c̄))
  -----------------------------------------------------------------------------
  [ρ| x[y..y] = s |] 
    | Just c <- AString.toChar (s ∧ Σ) 
    -> normRelA $ [ρ| x[y] = c |]
  -----------------------------------------------------------------------------
  [ρ| x[y..y] ≠ s |] 
    | Just c <- AString.toChar (s ∧ Σ)
    -> normRelA $ [ρ| x[y] ≠ c |]
  -----------------------------------------------------------------------------
  [ρ| x[î..ĵ] = s |]
    | [i] <- î, [j] <- ĵ
    , i >= 0, i <= j
    -> normRelA $ x :=: EStrA (rep Σ i ⋅ (s ∧ rep Σ (j - i + 1)) ⋅ star Σ)
  -----------------------------------------------------------------------------
  [ρ| x[î..|x|-1] = s |]
    | [i] <- î
    , i >= 0
    -> normRelA $ x :=: EStrA (rep Σ i ⋅ s)
  -----------------------------------------------------------------------------
  [ρ| x[î..|x|-1] ≠ s |]
    | [i] <- î
    , i >= 0
    -> normRelA $ x :=: EStrA (rep Σ i ⋅ neg s)
  -----------------------------------------------------------------------------
  [ρ| x[î] = c |] 
    | [i] <- î
    , i >= 0 
    -> normRelA $ x :=: EStrA (rep Σ i ⋅ lit c ⋅ star Σ)
  -----------------------------------------------------------------------------
  [ρ| x[î] = c |]
    | [Fin m  :… PosInf] <- AInt.intervals î
    , let t = rep Σ m ⋅ star Σ ⋅ lit c ⋅ star Σ
    -> normRelA [ρ| x = t |]
  -----------------------------------------------------------------------------
  [ρ| x[y] ≠ c |] -> normRelA [ρ| x[y] = c̄ |] where c̄ = neg c
  -----------------------------------------------------------------------------
  [ρ| re.comp(x) = re.comp(y) |] -> normRelA $ x :=: y
  [ρ| re.comp(x) ≠ re.comp(y) |] -> normRelA $ x :≠: y
  [ρ| re.comp(x) = y          |] -> normRelA $ x :≠: y
  -----------------------------------------------------------------------------
  Relℤ y [ρ| x[y] = a |] :=: Relℤ z [ρ| x[z] = b |]
    -> normRelA $ x :=: EStrA (strWithCharGapChar a 0 b)
  -----------------------------------------------------------------------------
  Relℤ y [ρ| x[y-1] = a |] :=: Relℤ z [ρ| x[z-1] = b |]
    -> normRelA $ x :=: EStrA (strWithCharGapChar a 0 b)
  -----------------------------------------------------------------------------
  Relℤ y [ρ| x[y] = a |] :=: Relℤ z [ρ| x[z+n̂] = b |]
    | [n] <- n̂
    -> normRelA $ x :=: EStrA (strWithCharGapChar a n b)
  -----------------------------------------------------------------------------
  Relℤ y [ρ| x[y+m̂] = a |] :=: Relℤ z [ρ| x[z] = b |]
    | [m] <- m̂
    -> normRelA $ x :=: EStrA (strWithCharGapChar a (0 - m) b)
  -----------------------------------------------------------------------------
  Relℤ y [ρ| x[y+m̂] = a |] :=: Relℤ z [ρ| x[z+n̂] = b |]
    | [m] <- m̂, [n] <- n̂
    -> normRelA $ x :=: EStrA (strWithCharGapChar a (n - m) b)
  -----------------------------------------------------------------------------
  Relℤ y [ρ| x[y-m̂] = a |] :=: Relℤ z [ρ| x[z-n̂] = b |]
    | AIntFrom 2 <- m̂ ∧ AIntFrom 1
    , AIntFrom 1 <- n̂ ∧ AIntFrom 1
    , let t₁ = lit a ⋅ star Σ ⋅ lit b
    , let t₂ = lit b ⋅ star Σ ⋅ lit a ⋅ Σ
    , let t₃ = lit (a ∧ b) ⋅ Σ
    , let t = star Σ ⋅ (t₁ ∨ t₂ ∨ t₃) ⋅ star Σ
    -> normRelA [ρ| x = t |]
  -----------------------------------------------------------------------------
  Relℤ y [ρ| x[y-m̂] = a |] :=: Relℤ z [ρ| x[z-n̂] = b |]
    | AIntFrom 1 <- m̂ ∧ AIntFrom 1
    , AIntFrom 1 <- n̂ ∧ AIntFrom 1
    , let t₁ = lit a ⋅ star Σ ⋅ lit b
    , let t₂ = lit b ⋅ star Σ ⋅ lit a
    , let t₃ = lit (a ∧ b)
    , let t = star Σ ⋅ (t₁ ∨ t₂ ∨ t₃) ⋅ star Σ
    -> normRelA [ρ| x = t |]
  -----------------------------------------------------------------------------
  Relℤ y [ρ| x[y-1] = a |] :=: Relℤ z [ρ| x[z-n̂] = b |]
    | AIntFrom 1 <- n̂ ∧ AIntFrom 1
    , let t₂ = lit b ⋅ star Σ ⋅ lit a
    , let t₃ = lit (a ∧ b)
    , let t = star Σ ⋅ (t₂ ∨ t₃) ⋅ star Σ
    -> normRelA [ρ| x = t |]
  -----------------------------------------------------------------------------
  Relℤ y [ρ| x[y+m̂] = a |] :=: Relℤ z [ρ| x[z-n̂] = b |]
    | AIntFrom 0 <- m̂ ∧ AIntFrom 0
    , AIntFrom 0 <- n̂ ∧ AIntFrom 0
    , let t₁ = lit (a ∧ b)
    , let t₂ = lit a ⋅ star Σ ⋅ lit b
    , let t = star Σ ⋅ (t₁ ∨ t₂) ⋅ star Σ
    -> normRelA [ρ| x = t |]
  -----------------------------------------------------------------------------
  Relℤ y [ρ| x[0..y - 1] = s |] :=: Relℤ z [ρ| x[z + 1..|x| - 1] = t |]
    -> normRelA $ x :=: EStrA (s ⋅ Σ ⋅ t)
  -----------------------------------------------------------------------------
  Relℤ y [ρ| x[0..y - 1] = s |] :=: Relℤ z [ρ| x[z + 1..|x| - 1] ≠ t |]
    -> normRelA $ x :=: EStrA (s ⋅ Σ ⋅ neg t)
  -----------------------------------------------------------------------------
  Relℤ y [ρ| x[y + 1..|x| - 1] = s |] :=: Relℤ z [ρ| x[z + 1..|x| - 1] = t |]
    -> normRelA $ x :=: EStrA (star Σ ⋅ (s ∧ t))
  -----------------------------------------------------------------------------
  Relℤ y [ρ| x[y + 1..|x| - 1] = s |] :=: Relℤ z [ρ| x[z + 1..|x| - 1] ≠ t |]
    -> normRelA $ x :=: EStrA (star Σ ⋅ (s ∧ neg t))
  -----------------------------------------------------------------------------
  Relℤ y [ρ| x[y + 1..|x| - 1] ≠ s |] :=: Relℤ z [ρ| x[z + 1..|x| - 1] = t |]
    -> normRelA $ x :=: EStrA (star Σ ⋅ (neg s ∧ t))
  -----------------------------------------------------------------------------
  Relℤ y [ρ| x[y + 1..|x| - 1] ≠ s |] :=: Relℤ z [ρ| x[z + 1..|x| - 1] ≠ t |]
    -> normRelA $ x :=: EStrA (star Σ ⋅ neg (s ∨ t))
  -----------------------------------------------------------------------------
  [ω| i |] :=: Relℤ y [ρ| x[y] = c |] 
    -> normRelA $ x :=: EStrA (strWithCharAt i c)
  -----------------------------------------------------------------------------
  [ω| i |] :=: Relℤ y [ρ| x[y - j] = c |] 
    -> normRelA $ x :=: EStrA (strWithCharAt (AInt.sub i j) c)
  -----------------------------------------------------------------------------
  [ω| i |] :=: Relℤ y [ρ| x[y + j] = c |] 
    -> normRelA $ x :=: EStrA (strWithCharAt (AInt.add i j) c)
  -----------------------------------------------------------------------------
  [ω| |x| - i |] :=: Relℤ y [ρ| x[y] = c |] 
    -> normRelA $ x :=: EStrA (strWithCharAtRev i c)
  -----------------------------------------------------------------------------
  (Relℤ y [ρ| x[y] = c |] :+: EIntA i) :=: [ω| |x| |]
    -> normRelA $ x :=: EStrA (strWithCharAtRev i c)
  -----------------------------------------------------------------------------
  [ω| |x| - i |] :=: Relℤ y [ρ| x[y - j] = c |]
    -> normRelA $ x :=: EStrA (strWithCharAtRev (AInt.add i j) c)
  -----------------------------------------------------------------------------
  [ω| |x| - i |] :=: Relℤ y [ρ| x[y + j] = c |]
    -> normRelA $ x :=: EStrA (strWithCharAtRev (AInt.sub i j) c)
  -----------------------------------------------------------------------------
  [ω| |x| + i |] :=: Relℤ y [ρ| x[y] = c |] 
    -> normRelA $ x :=: EStrA (strWithCharAtRev (AInt.negate i) c)
  -----------------------------------------------------------------------------
  EIntA (AIntFrom 0) :=: Relℤ y [ρ| mod(y,n̂) = m̂ |]
    | [n] <- n̂, [m] <- m̂
    , n >= 0, m >= 0
    -> Left True
  -----------------------------------------------------------------------------
  Relℤ y [ρ| x[y] = c |] :=: Relℤ z [ρ| z ≠ |x| |]
    -> normRelA $ x :=: EStrA (star Σ ⋅ lit c ⋅ star Σ)
  -----------------------------------------------------------------------------
  Relℤ y [ρ| x[y+1..|x|-1] = s |] :=: Relℤ z [ρ| x[0..z-1] = t |]
    -> normRelA $ x :=: EStrA (t ⋅ s)
  -----------------------------------------------------------------------------
  EIntA (AIntFrom 0) :=: Relℤ y [ρ| x[y+î..|x|-1] = s |]
    | [i] <- AInt.values î
    -> normRelA $ x :=: EStrA (star Σ ⋅ rep Σ i ⋅ s)
  -----------------------------------------------------------------------------
  EIntA (AIntFrom 1) :=: Relℤ y [ρ| x[0..y-1] = s |]
    -> normRelA $ x :=: EStrA (s ⋅ star Σ)
  -----------------------------------------------------------------------------
  EIntA (AIntFrom 1) :=: Relℤ y [ρ| x[0..y-1] ≠ s |]
    -> normRelA $ x :=: EStrA (neg s ⋅ star Σ)
  -----------------------------------------------------------------------------
  EIntA (AIntFrom 0) :=: Relℤ y [ρ| x[î..y-1] = s |]
    | [i] <- î
    -> normRelA $ x :=: EStrA (rep Σ i ⋅ s ⋅ star Σ)
  -----------------------------------------------------------------------------
  EIntA (AIntFrom k) :=: Relℤ y [ρ| x[y..|x|-1] = s |]
    -> normRelA $ x :=: EStrA (rep Σ k ⋅ star Σ ⋅ s)
  -----------------------------------------------------------------------------
  EIntA (AIntFrom k) :=: Relℤ y [ρ| x[y+1..|x|-1] = s |]
    -> normRelA $ x :=: EStrA (rep Σ (k + 1) ⋅ star Σ ⋅ s)
  -----------------------------------------------------------------------------
  EIntA (AIntFrom k) :=: Relℤ y [ρ| x[y+1..|x|-1] ≠ s |]
    -> normRelA $ x :=: EStrA (rep Σ (k + 1) ⋅ star Σ ⋅ neg s)
  -----------------------------------------------------------------------------
  EIntA (AIntFrom 0) :=: Relℤ y [ρ| x[î..y-1] = s |]
    | [i] <- î    
    -> normRelA $ x :=: EStrA (rep Σ i ⋅ s ⋅ star Σ)
  -----------------------------------------------------------------------------
  EIntA (AIntFrom 0) :=: Relℤ y [ρ| x[î..y-1] ≠ s |]
    | [i] <- î
    , let s̄ = neg s
    -> normRelA $ x :=: EStrA (rep Σ i ⋅ s̄ ⋅ star Σ)
  -----------------------------------------------------------------------------
  [ω| |x| + ĵ |] :=: Relℤ y [ρ| x[î..y-1] = s |]
    | [i] <- î
    , [-1,0] <- ĵ ∧ AIntTo 0
    -> normRelA $ x :=: EStrA (rep Σ i ⋅ s ⋅ opt Σ)
  -----------------------------------------------------------------------------
  [ω| |x| + i |] :=: Relℤ y [ρ| x[0..y-1] = s |]
    | Just n <- strLen1 s
    , let n̂ = AInt.eq n
    -> normRelA [ρ| |x| + i = n̂ |]
  -----------------------------------------------------------------------------
  [ρ| x[î..str.indexof(x,s,î)-1] = t |]
    | [i] <- î
    , let s̄ = neg (star Σ ⋅ s ⋅ star Σ)
    , let t' = t ∧ s̄
    -> normRelA $ x :=: EStrA (rep Σ i ⋅ t' ⋅ s ⋅ star Σ)
  -----------------------------------------------------------------------------
  [ρ| x[î..str.indexof(x,s,î)-1] ≠ t |]
    | [i] <- î
    , let s̄ = neg (star Σ ⋅ s ⋅ star Σ)
    , let t' = (neg t) ∧ s̄
    -> normRelA $ x :=: EStrA (rep Σ i ⋅ t' ⋅ s ⋅ star Σ)
  -----------------------------------------------------------------------------
  [ρ| x[str.indexof(x,s,î)..|x|-1] = t |]
    | [i] <- î
    , let s̄ = neg (star Σ ⋅ s ⋅ star Σ)
    -> normRelA $ x :=: EStrA (rep Σ i ⋅ s̄ ⋅ t)
  -----------------------------------------------------------------------------
  [ρ| x[str.indexof(x,s,î)..|x|-1] ≠ t |]
    | [i] <- î
    , let s̄ = neg (star Σ ⋅ s ⋅ star Σ)
    -> normRelA $ x :=: EStrA (rep Σ i ⋅ s̄ ⋅ neg t)
  -----------------------------------------------------------------------------
  [ρ| x[|x|-î..|x|-ĵ] = s |]
    | [i] <- î
    , [j] <- ĵ, j >= 1, j <= i
    , let s' = s ∧ rep Σ (i - j + 1)
    -> normRelA $ x :=: EStrA (star Σ ⋅ s' ⋅ rep Σ (j - 1))
  -----------------------------------------------------------------------------
  [ρ| |x| = i |] | let i' = i ∧ AInt.ge 0, i' /= i -> normRelA [ρ| |x| = i' |]
  ----------------------------------------------------------------------------
  x :=: ERelA v _ r | concreteish x -> normRelA $ subst x v r
  -----------------------------------------------------------------------------
  r -> Right r

-------------------------------------------------------------------------------

-- | Isolate a variable on the left-hand side of a relation, if possible.
isolate :: Name -> ARel -> ARel
isolate x r | occurrences x r /= 1 = r
isolate x r = flip rewrite r $ \case
  ω₁ :=: ω₂            | x `freeIn` ω₂ -> Just $ ω₂ :=: ω₁
  ω₁ :≠: ω₂            | x `freeIn` ω₂ -> Just $ ω₂ :≠: ω₁
  ω₁ :<: ω₂            | x `freeIn` ω₂ -> Just $ ω₂ :>: ω₁
  ω₁ :≤: ω₂            | x `freeIn` ω₂ -> Just $ ω₂ :≥: ω₁
  ω₁ :>: ω₂            | x `freeIn` ω₂ -> Just $ ω₂ :<: ω₁
  ω₁ :≥: ω₂            | x `freeIn` ω₂ -> Just $ ω₂ :≤: ω₁
  Rel o (ω₁ :+: ω₂) ω₃ | x `freeIn` ω₁ -> Just $ Rel o ω₁ (ω₃ :-: ω₂)
  Rel o (ω₁ :+: ω₂) ω₃ | x `freeIn` ω₂ -> Just $ Rel o ω₂ (ω₃ :-: ω₁)
  Rel o (ω₁ :-: ω₂) ω₃ | x `freeIn` ω₁ -> Just $ Rel o ω₁ (ω₃ :+: ω₂)
  Rel o (ω₁ :-: ω₂) ω₃ | x `freeIn` ω₂ -> Just $ Rel o ω₂ (ω₁ :-: ω₃)
  _                                    -> Nothing

pattern ARel𝕊 :: AExpr -> ARel -> AValue
pattern ARel𝕊 x r <- (matchARel TString -> Just (x,r)) where
  ARel𝕊 (EVar x _) r = ARel x TString r
  ARel𝕊 _ _ = undefined

isDeferredStrComp :: AValue -> Bool
isDeferredStrComp (AStringComp _) = True
isDeferredStrComp _ = False

pattern AStringComp :: AString -> AValue
pattern AStringComp s <- ARel𝕊 x [ρ| x = re.comp(s) |] where
  AStringComp s = ARel𝕊 x [ρ| x = re.comp(s) |] 
                    where x = EVar "x" TString

pattern ARelℤ :: AExpr -> ARel -> AValue
pattern ARelℤ x r <- (matchARel TInt -> Just (x,r)) where
  ARelℤ (EVar x _) r = ARel x TInt r
  ARelℤ _ _ = undefined

matchARel :: Base -> AValue -> Maybe (AExpr, ARel)
matchARel b (ARel v b1 r) | b == b1 = Just (EVar v b, r)
matchARel _ _ = Nothing

instance PartialMeetSemilattice AValue where
  AUnit   a ∧? AUnit   b = Just $ AUnit   (a ∧ b)
  ABool   a ∧? ABool   b = Just $ ABool   (a ∧ b)
  AInt    a ∧? AInt    b = Just $ AInt    (a ∧ b)
  AChar   a ∧? AChar   b = Just $ AChar   (a ∧ b)
  AString a ∧? AString b = Just $ AString (a ∧ b)
  -----------------------------------------------------------------------------
  ARel𝕊 x [ρ| x = re.comp(s) |] ∧? ARel𝕊 y [ρ| y = re.comp(t) |]
    = Just $ ARel𝕊 x (x :=: EStrComp (EStrA (s ∨ t)))
  -----------------------------------------------------------------------------
  ARel𝕊 x [ρ| x = re.comp(s) |] ∧? AString t
    = Just $ ARel𝕊 x (x :=: EStrComp (EStrA (s ∨ neg t)))
  -----------------------------------------------------------------------------
  AString t ∧? ARel𝕊 x [ρ| x = re.comp(s) |]
    = Just $ ARel𝕊 x (x :=: EStrComp (EStrA (s ∨ neg t)))
  -----------------------------------------------------------------------------
  ARelℤ y [ρ| y = x + i |] ∧? ARelℤ z [ρ| z = x + j |]
    | isBot k   = Just $ AInt bot
    | otherwise = Just $ ARelℤ y [ρ| y = x + k |] 
    where k = i ∧ j
  -----------------------------------------------------------------------------
  ARelℤ y [ρ| y = x - i |] ∧? ARelℤ z [ρ| z = x + j |]
    | isBot k   = Just $ AInt bot
    | otherwise = Just $ ARelℤ y [ρ| y = x + k |] 
    where k = AInt.negate i ∧ j
  -----------------------------------------------------------------------------
  ARelℤ y [ρ| y = x + i |] ∧? ARelℤ z [ρ| z = x - j |]
    | isBot k   = Just $ AInt bot
    | otherwise = Just $ ARelℤ y [ρ| y = x + k |] 
    where k = i ∧ AInt.negate j
  -----------------------------------------------------------------------------
  ARelℤ y [ρ| y = x - i |] ∧? ARelℤ z [ρ| z = x - j |]
    | isBot k   = Just $ AInt bot
    | otherwise = Just $ ARelℤ y [ρ| y = x - k |] 
    where k = i ∧ j
  -----------------------------------------------------------------------------
  ARelℤ y [ρ| y = x + i |] ∧? ARelℤ z [ρ| z = x |]
    | isBot k   = Just $ AInt bot
    | otherwise = Just $ ARelℤ y [ρ| y = x + k |] 
    where k = i ∧ AInt.eq 0
  -----------------------------------------------------------------------------
  ARelℤ y [ρ| y = x - i |] ∧? ARelℤ z [ρ| z = x |]
    | isBot k   = Just $ AInt bot
    | otherwise = Just $ ARelℤ y [ρ| y = x - k |] 
    where k = i ∧ AInt.eq 0
  -----------------------------------------------------------------------------
  ARelℤ y [ρ| y = x |] ∧? ARelℤ z [ρ| z = x + i |]
    | isBot k   = Just $ AInt bot
    | otherwise = Just $ ARelℤ y [ρ| y = x + k |] 
    where k = i ∧ AInt.eq 0
  -----------------------------------------------------------------------------
  ARelℤ y [ρ| y = x |] ∧? ARelℤ z [ρ| z = x - i |]
    | isBot k   = Just $ AInt bot
    | otherwise = Just $ ARelℤ y [ρ| y = x - k |] 
    where k = i ∧ AInt.eq 0
  -----------------------------------------------------------------------------
  a ∧? b = if a == b then Just a else Nothing

instance PartialJoinSemilattice AValue where
  AUnit   a ∨? AUnit   b = Just $ AUnit   (a ∨ b)
  ABool   a ∨? ABool   b = Just $ ABool   (a ∨ b)
  AInt    a ∨? AInt    b = Just $ AInt    (a ∨ b)
  AChar   a ∨? AChar   b = Just $ AChar   (a ∨ b)
  AString a ∨? AString b = Just $ AString (a ∨ b)
  -----------------------------------------------------------------------------
  ARel𝕊 x [ρ| x = re.comp(s) |] ∨? ARel𝕊 y [ρ| y = re.comp(t) |]
    = Just $ ARel𝕊 x (x :=: EStrComp (EStrA (s ∧ t)))
  -----------------------------------------------------------------------------
  ARel𝕊 x [ρ| x = re.comp(s) |] ∨? AString t
    = Just $ ARel𝕊 x (x :=: EStrComp (EStrA (s ∧ neg t)))
  -----------------------------------------------------------------------------
  AString t ∨? ARel𝕊 x [ρ| x = re.comp(s) |]
    = Just $ ARel𝕊 x (x :=: EStrComp (EStrA (s ∧ neg t)))
  -----------------------------------------------------------------------------
  a         ∨? b         = if a == b then Just a else Nothing

-- | Variable-focused abstract semantics function ⟦ρ⟧↑x.
--
-- For a given variable x of base type b occurring free in the relation ρ,
-- @abstract x b ρ@ produces an abstract expression whose concrete values are
-- exactly those that could be substituted for x to make ρ true, i.e.,
--
--    𝔐, [x ↦ c] ⊧ ρ  ⟺  c ∈ ⟦ρ⟧↑x.
--
-- Abstract relations 'ERelA' provide a convenient "default" implementation,
--
--    ⟦ρ⟧↑x ≐ ⟨x: ρ⟩.
--
abstract :: Name -> Base -> ARel -> AValue
-- abstract x b r0 = case normRelA r0 of
abstract x τ r0 = trace ("abstract " ++ showPretty x ++ " " ++ showPretty r0 ++ " " ++ showPretty (freeVars r0)) $ case normRelA r0 of
 Left True  -> topValue τ
 Left False -> botValue τ
 Right r    -> go r 
 where
 x̲ = EVar x τ
 go = \case
  -----------------------------------------------------------------------------
  r | x `notFreeIn` r  -> ARel x τ r
  -----------------------------------------------------------------------------
  ω₁ :=: ω₂ | x `notFreeIn` ω₁ -> go $ ω₂ :=: ω₁
  ω₁ :≠: ω₂ | x `notFreeIn` ω₁ -> go $ ω₂ :≠: ω₁
  -----------------------------------------------------------------------------
  -- NOTE: below here, x occurs on the LHS and may also occur on the RHS
  -----------------------------------------------------------------------------
  [ρ| str.indexof(x̲,c,0) + i = |x̲| |] -> AString $ strWithFirstIndexOfCharRev c i
  -----------------------------------------------------------------------------
  [ρ| str.indexof(x̲,a,0) + n = str.indexof(x̲,b,0) |]
    | AIntFrom 0 <- n, a /= b, let ā = neg a, let b̄ = neg b
    -> AString $ star (lit (ā ∧ b̄)) ⋅ opt ((lit b ⋅ star (lit ā)) ∨ (lit a ⋅ star Σ))
  -----------------------------------------------------------------------------
  [ρ| str.indexof(x̲,a,0) - n = str.indexof(x̲,b,0) |]
    | AIntTo 1 <- n, a /= b, let ā = neg a, let b̄ = neg b
    -> AString $ star (lit (ā ∧ b̄)) ⋅ opt ((lit b ⋅ star (lit ā)) ∨ (lit a ⋅ star Σ))
  -----------------------------------------------------------------------------
  r@(ω₁ :=: ω₂) | x `freeIn` ω₁, x `freeIn` ω₂ -> ARel x τ r
  r@(ω₁ :≠: ω₂) | x `freeIn` ω₁, x `freeIn` ω₂ -> ARel x τ r
  -----------------------------------------------------------------------------
  -- NOTE: below here, x occurs only on the LHS (possibly more than once)
  -----------------------------------------------------------------------------
  [ρ| x̲ = _1 |] -> AUnit _1
  [ρ| x̲ = p  |] -> ABool p
  [ρ| x̲ = n  |] -> AInt n
  [ρ| x̲ = s  |] -> AString s
  [ρ| x̲ = c  |] -> AChar c 
  -- NOTE: The char case needs to come after the string case here, so that it
  -- doesn't accidentally match one-character singleton strings.
  -----------------------------------------------------------------------------  
  [ρ| x̲ ≠ _1 |] -> AUnit (neg _1)
  [ρ| x̲ ≠ p  |] -> ABool (neg p)
  [ρ| x̲ ≠ n  |] -> AInt (neg n)
  [ρ| x̲ ≠ s  |] -> ARel x τ [ρ| x̲ = re.comp(s) |] --AString (neg s)
  [ρ| x̲ ≠ c  |] -> AChar (neg c)  -- see note above
  -----------------------------------------------------------------------------  
  [ρ| x̲ ≠ e |]
    | τ == TBool   -> abstract x τ [ρ| x̲ = not(e)      |]               
    | τ == TInt    -> let k = AInt.ne 0 in abstract x τ [ρ| x̲ = e + k |]
    | τ == TString -> abstract x τ [ρ| x̲ = re.comp(e) |]
  -----------------------------------------------------------------------------
  [ρ| x̲ = re.comp(s) |] -> AString (neg s)
  -- NOTE: String complement is resolved here instead of during normalization,
  -- in order to exploit opportunities for double-negation elimination.
  -----------------------------------------------------------------------------
  [ρ| x̲ + n = e |] -> abstract x τ [ρ| x̲ = e - n |]
  [ρ| x̲ - n = e |] -> abstract x τ [ρ| x̲ = e + n |]
  -----------------------------------------------------------------------------
  [ρ| |x̲| = n |] -> AString $ strOfLen n
  [ρ| |x̲| ≠ n |] -> AString $ neg $ strOfLen n
  -----------------------------------------------------------------------------
  [ρ| x̲[i] = c |] -> AString $ strWithCharAt i c
  [ρ| x̲[i] ≠ c |] -> AString $ neg $ strWithCharAt i c
  -----------------------------------------------------------------------------
  [ρ| x̲[|x̲|-i] = c |] -> AString $ strWithCharAtRev i c
  [ρ| x̲[|x̲|-i] ≠ c |] -> AString $ neg $ strWithCharAtRev i c
  -----------------------------------------------------------------------------
  [ρ| x̲[i..j] = t |] -> AString $ strWithSubstr i j t
  [ρ| x̲[i..j] ≠ t |] -> AString $ neg $ strWithSubstr i j t
  -----------------------------------------------------------------------------
  [ρ| str.indexof(x̲,c,k) = i |] -> AString $ strWithIndexOfChar c k i
  -----------------------------------------------------------------------------
  [ρ| x̲[î..str.indexof(x̲,c,0)-ĵ] = t |]
    | [i] <- î, [j] <- ĵ
    , i >= 0, j >= 0, let c̄ = lit (neg c)
    -> AString $ rep c̄ i ⋅ (t ∧ star c̄) ⋅ rep c̄ (j-1) ⋅ lit c ⋅ star Σ
  -----------------------------------------------------------------------------
  [ρ| x̲[str.indexof(x̲,c,0)-î..|x̲|-ĵ] = t |]
    | [i] <- î, [j] <- ĵ
    -> AString $ strWithSubstrFromFirstIndexOfCharToEnd c i j t
  -----------------------------------------------------------------------------
  [ρ| str.indexof(x̲,a,str.indexof(x̲,b,0)+1) = i |]
    -> AString $ strWithFirstIndexOfCharFollowedByFirstIndexOfChar b a i
  -----------------------------------------------------------------------------
  [ρ| x̲[str.indexof(x̲,a,î)+n̂] = b |]
    | [i] <- î, [n] <- n̂
    , let ā = lit (neg a)
    -> AString $ rep Σ i ⋅ star ā ⋅ lit a ⋅ rep Σ (n - 1) ⋅ lit b ⋅ star Σ
  -----------------------------------------------------------------------------
  [ρ| str.contains(x̲,s) ≠ p |]
    -> abstract x τ [ρ| str.contains(x̲,s) = p̄ |] where p̄ = neg p
  -----------------------------------------------------------------------------
  [ρ| str.contains(x̲,s) = p̂ |]
    | [True]  <- p̂ -> AString t
    | [False] <- p̂ -> abstract x τ [ρ| x̲ = re.comp(t) |]
   where
    t = star Σ ⋅ s ⋅ star Σ
  -----------------------------------------------------------------------------
  r -> ARel x τ (isolate x r)

-------------------------------------------------------------------------------

concretizeUnit :: Name -> AUnit -> Pred
concretizeUnit x a = case a of
  AUnit.Unit   -> PRel $ EVar x TUnit :=: EUnit NoPV
  AUnit.Bottom -> PFalse

concretizeBool :: Name -> ABool -> Pred
concretizeBool x a = case ABool.value a of
  Just b  -> PRel $ EVar x TBool :=: EBool b NoPV
  Nothing -> if isTop a then PTrue else PFalse

concretizeInt :: Name -> AInt -> Pred
concretizeInt x a = case AInt.intervals a of
  []                                    -> PFalse  
  [NegInf :… PosInf]                    -> PTrue
  [NegInf :… Fin n ]                    -> mk (:≤:) n
  [Fin m  :… PosInf]                    -> mk (:≥:) m
  [Fin m  :… Fin n ] | m == n           -> mk (:=:) m
                     | otherwise        -> mk (:≥:) m ∧ mk (:≤:) n
  [NegInf :… Fin m, Fin n :… PosInf]
                     | n - m == 2       -> mk (:≠:) (m + 1)
                     | otherwise        -> mk (:≤:) m ∨ mk (:≥:) n
  (Fin m  :… _) : (last -> _ :… Fin n ) -> mk (:≥:) m ∧ mk (:≤:) n ∧ mkHoles
  (NegInf :… _) : (last -> _ :… Fin n ) -> mk (:≤:) n ∧ mkHoles
  (Fin m  :… _) : (last -> _ :… PosInf) -> mk (:≥:) m ∧ mkHoles
  (NegInf :… _) : (last -> _ :… PosInf) -> mkHoles
  _                                     -> impossible
 where
  mk op n = PRel $ op (EVar x TInt) (EInt (fromIntegral n) NoPV)
  mkHoles = meets $ map (mk (:≠:)) $ AInt.holes $ AInt.intervals a

concretizeChar :: Name -> AChar -> Pred
concretizeChar x ĉ
  | [c] <- AChar.values (neg ĉ) = PRel $ EVar x TChar :≠: EChar c NoPV
  | isBot ĉ   = PFalse
  | isTop ĉ   = PTrue
  | otherwise = joins $ [PRel $ EVar x TChar :=: EChar c NoPV | c <- AChar.values ĉ]

concretizeString :: Name -> AString -> Pred
concretizeString x a = case AString.toRegex a of
  Regex.Zero -> PFalse
  Regex.One  -> PRel $ EVar x TString :=: EStr "" NoPV
  Regex.All  -> PTrue
  r -> case Regex.POSIX.ERE.fromRegex r of
    Just ere -> PRel $ EVar x TString :∈: EReg ere
    Nothing  -> panic $ "cannot convert Regex to ERE:" <+> prettyRegex r
