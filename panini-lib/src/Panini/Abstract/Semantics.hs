{-# LANGUAGE OverloadedLists #-}
module Panini.Abstract.Semantics
  ( normRelA
  , normExprA
  , abstract
  , concretizeUnit
  , concretizeBool
  , concretizeInt
  , concretizeChar
  , concretizeString
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

-- | abstract unit constant
pattern U :: AUnit -> AExpr
pattern U u = EVal (AUnit u)

-- | abstract Boolean constant
pattern B :: ABool -> AExpr
pattern B p = EVal (ABool p)

-- | abstract integer constant
pattern Z :: AInt -> AExpr
pattern Z n = EVal (AInt n)

-- | Match any abstract integer but return only its positive part, including 0.
pattern Z⁰ :: AInt -> AExpr
pattern Z⁰ n̂ <- Z (meet (AInt.ge 0) -> n̂)

-- | Match any abstract integer but return only its positive part, excluding 0.
pattern Z¹ :: AInt -> AExpr
pattern Z¹ n̂ <- Z (meet (AInt.ge 1) -> n̂)

-- | abstract integer addition
(⊕) :: AInt -> AInt -> AInt
(⊕) = AInt.add

-- | abstract integer subtraction
(⊖) :: AInt -> AInt -> AInt
(⊖) = AInt.sub

-- | abstract integer comparison with concrete integer
(⋖) :: AInt -> Integer -> Bool
(⋖) = AInt.isLe

-- | abstract character constant; also matches one-character singleton strings
pattern C :: AChar -> AExpr
pattern C c <- EVal (matchChar -> Just c) where
  C c = EVal (AChar c)

matchChar :: AValue -> Maybe AChar
matchChar (AChar c)   = Just c
matchChar (AString s) = AString.toChar s
matchChar _           = Nothing

-- | abstract string constant
pattern S :: AString -> AExpr
pattern S s = EVal (AString s)

pattern V :: Name -> AExpr
pattern V x <- EVar x _

-- | Match both x+n and x; in the latter case, n is taken to be 0.
pattern (:⨤:) :: Name -> Integer -> AExpr
pattern x :⨤: n <- (exprToVarPlusN -> Just (x,n))

exprToVarPlusN :: AExpr -> Maybe (Name, Integer)
exprToVarPlusN = \case
  EVar x TInt           -> Just (x, 0)
  EVar x TInt :+: Z [n] -> Just (x, n)
  _                     -> Nothing

-------------------------------------------------------------------------------

-- | Normalize an abstract expression by (partial) evaluation; see 'normRel'.
normExprA :: AExpr -> AExpr
normExprA = rewrite $ \case
  -----------------------------------------------------------------------------
  ERelA x₁ _ (isolate x₁ -> V x₂ :=: ω) | x₁ == x₂, x₁ `notFreeIn` ω -> Just ω
  -----------------------------------------------------------------------------
  ERelA x b ρ -> case normRelA ρ of
    Left True            -> Just $ EAbs (topValue b)
    Left False           -> Just $ EAbs (botValue b)
    Right ρ' | ρ' /= ρ   -> Just $ ERelA x b ρ'
             | otherwise -> case abstract x b ρ of
                              ARel _ _ _ -> Nothing
                              a          -> Just $ EAbs a
  -----------------------------------------------------------------------------
  ENot (B a)    -> Just $ B (neg a)
  ENot (ENot ω) -> Just ω
  -----------------------------------------------------------------------------
  Z a :+: _    | isBot a -> Just $ Z a
  Z a :-: _    | isBot a -> Just $ Z a
  _   :+: Z a  | isBot a -> Just $ Z a
  _   :-: Z a  | isBot a -> Just $ Z a
  Z a :+: Z b            -> Just $ Z (a ⊕ b)
  Z a :-: Z b            -> Just $ Z (a ⊖ b)
  ω   :+: Z [0]          -> Just ω
  ω   :-: Z [0]          -> Just ω
  ω   :+: Z a  | a ⋖ 0   -> Just $ ω :-: Z (AInt.negate a)
  ω   :-: Z a  | a ⋖ 0   -> Just $ ω :+: Z (AInt.negate a)
  -----------------------------------------------------------------------------
  (ω   :+: Z a) :+: Z b -> Just $ ω :+: Z (a ⊕ b)
  (ω   :+: Z a) :-: Z b -> Just $ ω :+: Z (a ⊖ b)
  (ω   :-: Z a) :+: Z b -> Just $ ω :-: Z (a ⊖ b)
  (ω   :-: Z a) :-: Z b -> Just $ ω :-: Z (a ⊕ b)
  (Z a :+: ω  ) :+: Z b -> Just $ ω :+: Z (a ⊕ b)
  (Z a :+: ω  ) :-: Z b -> Just $ ω :+: Z (a ⊖ b)
  (Z a :-: ω  ) :+: Z b -> Just $ Z (a ⊕ b) :-: ω
  (Z a :-: ω  ) :-: Z b -> Just $ Z (a ⊖ b) :-: ω
  -----------------------------------------------------------------------------
  EMod (Z [a]) (Z [b]) -> Just $ Z [a `mod` b]
  -----------------------------------------------------------------------------
  EStrLen (S s) | isTop s             -> Just $ Z (AInt.ge 0)
                | Just n <- strLen1 s -> Just $ Z [n]
  -- NOTE: We don't have any efficient way to compute nor represent, in general,
  -- the precise lengths of all strings contained in an abstract string.
  -----------------------------------------------------------------------------
  EStrAt (S (AString1 s)) (Z i)             -> Just $ C (charAt s i)
  EStrAt (S s) (Z [i])                      -> Just $ C (charsAt s i)
  EStrAt (V x₁) (EStrLen (V x₂)) | x₁ == x₂ -> Just $ C bot
  -----------------------------------------------------------------------------
  EStrAt s₁ (EStrLen s₂ :+: Z n) 
    | s₁ == s₂, let n' = n ∧ AInt.lt 0, n' /= n 
    -> Just $ EStrAt s₁ (EStrLen s₂ :-: Z (AInt.negate n'))
  -----------------------------------------------------------------------------
  EStrSub (S (AString1 s)) (Z i) (Z j)                    -> Just $ S (strSub s i j)
  EStrSub (S s) (Z [i₁]) (Z [i₂]) | i₁ == i₂              -> Just $ S (lit $ charsAt s i₁)
  EStrSub    ω₁ (Z [0]) (EStrLen ω₂ :-: Z [1]) | ω₁ == ω₂ -> Just ω₁
  -----------------------------------------------------------------------------
  EStrSub (EStrSub s (Z [i]) (Z [j])) (Z [k]) (Z [l])
    | i >= 0, i <= j, k >= 0, k <= l, l - k <= j - i
    -> Just $ EStrSub s (Z [i + k]) (Z [i + k + (l - k)])
  -----------------------------------------------------------------------------
  EStrComp (EStrComp ω) -> Just ω
  -- EStrComp (S s) -> Just $ S (neg s)
  -- NOTE: We want to defer resolution of EStrComp as long as possible,
  -- in order to exploit opportunities for double-negation cancellation!
  -----------------------------------------------------------------------------
  EStrConc (S a) (S b) -> Just $ S (a ⋅ b)
  EStrConc (EStrSub ω₁ (Z [i₁]) (Z [j₁])) (EStrSub ω₂ (Z [i₂]) (Z [j₂]))
    | ω₁ == ω₂, i₁ <= j₁, j₁ + 1 == i₂, i₂ <= j₂
    -> Just $ EStrSub ω₁ (Z [i₁]) (Z [j₂])
  -----------------------------------------------------------------------------
  EStrStar (S s) -> Just $ S (star s)
  -----------------------------------------------------------------------------
  EStrContains (S ŝ₁) (S ŝ₂) -> Just $ B [ŝ₂ ⊑ ŝ₁]
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
  ρ | ρ' <- descendBi normExprA ρ, ρ' /= ρ -> normRelA ρ'
  -----------------------------------------------------------------------------
  x :≠: Z [n] -> normRelA $ x :=: Z (AInt.ne n)
  x :<: Z [n] -> normRelA $ x :=: Z (AInt.lt n)
  x :≤: Z [n] -> normRelA $ x :=: Z (AInt.le n)
  x :>: Z [n] -> normRelA $ x :=: Z (AInt.gt n)
  x :≥: Z [n] -> normRelA $ x :=: Z (AInt.ge n)
  -----------------------------------------------------------------------------
  x :<: y -> normRelA $ x :=: (y :-: Z (AInt.gt 0))
  x :≤: y -> normRelA $ x :=: (y :-: Z (AInt.ge 0))
  x :>: y -> normRelA $ x :=: (y :+: Z (AInt.gt 0))
  x :≥: y -> normRelA $ x :=: (y :+: Z (AInt.ge 0))
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
  x :=: _ | anyBot x -> Left False
  _ :=: x | anyBot x -> Left False
  x :≠: _ | anyBot x -> Left True
  _ :≠: x | anyBot x -> Left True
  -----------------------------------------------------------------------------
  x           :=: y           | x == y -> Left True
  x           :=: (y :+: Z n) | x == y -> Left (AInt.member 0 n)
  x           :=: (y :-: Z n) | x == y -> Left (AInt.member 0 n)
  (x :+: Z n) :=: y           | x == y -> Left (AInt.member 0 n)
  (x :-: Z n) :=: y           | x == y -> Left (AInt.member 0 n)
  -----------------------------------------------------------------------------
  x           :≠: y           | x == y -> Left False
  x           :≠: (y :+: Z n) | x == y -> Left (not $ AInt.member 0 n)
  x           :≠: (y :-: Z n) | x == y -> Left (not $ AInt.member 0 n)
  (x :+: Z n) :≠: y           | x == y -> Left (not $ AInt.member 0 n)
  (x :-: Z n) :≠: y           | x == y -> Left (not $ AInt.member 0 n)
  -----------------------------------------------------------------------------
  Rel o (Z a :+: x) (Z b)       -> normRelA $ Rel o x (Z (b ⊖ a))
  Rel o (Z a :+: x) (Z b :+: y) -> normRelA $ Rel o x (y :+: Z (b ⊖ a))
  Rel o (Z a :+: x) (y :+: Z b) -> normRelA $ Rel o x (y :+: Z (b ⊖ a))
  Rel o (Z a :+: x) (Z b :-: y) -> normRelA $ Rel o x (Z (b ⊖ a) :-: y)
  Rel o (Z a :+: x) (y :-: Z b) -> normRelA $ Rel o x (y :-: Z (a ⊕ b))
  Rel o (Z a :-: x) (Z b)       -> normRelA $ Rel o x (Z (a ⊖ b))
  Rel o (Z a :-: x) (Z b :+: y) -> normRelA $ Rel o x (Z (a ⊖ b) :-: y)
  Rel o (Z a :-: x) (y :+: Z b) -> normRelA $ Rel o x (Z (a ⊖ b) :-: y)
  Rel o (Z a :-: x) (Z b :-: y) -> normRelA $ Rel o x (y :+: Z (a ⊖ b))
  Rel o (Z a :-: x) (y :-: Z b) -> normRelA $ Rel o x (Z (a ⊕ b) :-: y)
  Rel o (x :+: Z a) (Z b)       -> normRelA $ Rel o x (Z (b ⊖ a))
  Rel o (x :+: Z a) (Z b :+: y) -> normRelA $ Rel o x (y :+: Z (b ⊖ a))
  Rel o (x :+: Z a) (y :+: Z b) -> normRelA $ Rel o x (y :+: Z (b ⊖ a))
  Rel o (x :+: Z a) (Z b :-: y) -> normRelA $ Rel o x (Z (b ⊖ a) :-: y)
  Rel o (x :+: Z a) (y :-: Z b) -> normRelA $ Rel o x (y :-: Z (a ⊕ b))
  Rel o (x :-: Z a) (Z b)       -> normRelA $ Rel o x (Z (a ⊕ b))
  Rel o (x :-: Z a) (Z b :+: y) -> normRelA $ Rel o x (y :+: Z (a ⊕ b))
  Rel o (x :-: Z a) (y :+: Z b) -> normRelA $ Rel o x (y :+: Z (a ⊕ b))
  Rel o (x :-: Z a) (Z b :-: y) -> normRelA $ Rel o x (Z (a ⊕ b) :-: y)
  Rel o (x :-: Z a) (y :-: Z b) -> normRelA $ Rel o x (y :+: Z (a ⊖ b))
  -----------------------------------------------------------------------------
  -- str.indexof(x,y,0) = n   ≡   str.indexof(x,y,0) = n ⊓ [-1,∞]
  EStrIndexOf x y (Z [0]) :=: Z n
    | let n' = n ∧ AIntFrom (-1), n' /= n
    -> normRelA $ EStrIndexOf x y (Z [0]) :=: Z n'
  -----------------------------------------------------------------------------  
  -- str.indexof(x,c,0) = n   ≡   x = strWithFirstIndexOfChar c n
  EStrIndexOf x (C c) (Z [0]) :=: Z n
    -> normRelA $ x :=: S (strWithFirstIndexOfChar c n)
  -----------------------------------------------------------------------------
  -- str.indexof(s,t,i) = [-∞,-1]   ≡   str.indexof(s,t,i) = -1
  EStrIndexOf s t i :=: Z (AIntTo (-1)) 
    -> normRelA $ EStrIndexOf s t i :=: Z [-1]
  -----------------------------------------------------------------------------
  -- str.indexof(x,y,|x|+[-i,+∞]) = z   ≡   str.indexof(x,y,|x|-[0,i]) = z
  EStrIndexOf x₁ y (EStrLen x₂ :+: Z n) :=: z 
    | x₁ == x₂
    , [Fin i :… PosInf] <- AInt.intervals n, i < 0
    , let n' = AInt.fromTo 0 (Prelude.negate i)
    -> normRelA $ EStrIndexOf x₁ y (EStrLen x₂ :-: Z n') :=: z
  -----------------------------------------------------------------------------
  -- str.indexof(x,c,[i,+∞]) = -1   ≡   x = ΣⁱΣ*c̄*
  EStrIndexOf x (C c) (Z (AIntFrom i)) :=: Z [-1]
    -> normRelA $ x :=: S (rep Σ i ⋅ star Σ ⋅ star (lit (neg c)))
  -----------------------------------------------------------------------------
  -- str.indexof(x,c,|x|-[i,+∞]) = -1   ≡   x = Σ*c̄ⁱc̄*
  EStrIndexOf x₁ (C c) (EStrLen x₂ :-: Z (AIntFrom i)) :=: Z [-1] | x₁ == x₂
    -> normRelA $ x₁ :=: S (star Σ ⋅ rep c̄ i ⋅ star c̄) where c̄ = lit (neg c)
  -----------------------------------------------------------------------------
  -- str.indexof(x,c,|x|-[0,1]) = -1   ≡   x = Σ*c̄?
  EStrIndexOf x₁ (C c) (EStrLen x₂ :-: Z [0,1]) :=: Z [-1] | x₁ == x₂
    -> normRelA $ x₁ :=: S (star Σ ⋅ opt (lit (neg c)))
  -----------------------------------------------------------------------------
  -- i = str.indexof(s,t,i)   ≡   s[i..i+|t|-1] = t
  i₁ :=: EStrIndexOf s t i₂ 
    | i₁ == i₂ 
    -> normRelA $ EStrSub s i₂ (i₂ :+: (EStrLen t :-: Z [1])) :=: t
  -----------------------------------------------------------------------------
  -- i + n = str.indexof(s,t,i)   ≡   i + (n ⊓ [0,∞]) = str.indexof(s,t,i)
  i₁ :+: Z n̂ :=: EStrIndexOf s t i₂ 
    | i₁ == i₂, let n̂' = n̂ ∧ AInt.ge 0, n̂' /= n̂ 
    -> normRelA $ i₁ :+: Z n̂' :=: EStrIndexOf s t i₂
  -----------------------------------------------------------------------------
  -- i - n = str.indexof(s,t,i)   ≡   i - (n ⊓ [-∞,0]) = str.indexof(s,t,i)
  i₁ :-: Z n̂ :=: EStrIndexOf s t i₂ 
    | i₁ == i₂, let n̂' = n̂ ∧ AInt.le 0, n̂' /= n̂ 
    -> normRelA $ i₁ :-: Z n̂' :=: EStrIndexOf s t i₂
  -----------------------------------------------------------------------------
  --- |s| + n = str.indexof s t i   ≡   s + (n ⊓ [-∞,-1]) = str.indexof(s,t,i)
  EStrLen s₁ :+: Z n̂ :=: EStrIndexOf s₂ t i 
    | s₁ == s₂, let n̂' = n̂ ∧ AInt.lt 0, n̂' /= n̂ 
    -> normRelA $ EStrLen s₁ :+: Z n̂' :=: EStrIndexOf s₂ t i
  -----------------------------------------------------------------------------
  -- i ≠ str.indexof(s,c,i)   ≡   i = str.indexof(s,c̄,0)
  i₁ :≠: EStrIndexOf s (C c) i₂ 
    | i₁ == i₂
    -> normRelA $ i₁ :=: EStrIndexOf s (C (neg c)) (Z [0])
  -----------------------------------------------------------------------------
  --- |x|-1 = str.indexof(x,c,i)   ≡   x = Σⁱc̄*c
  EStrLen x₁ :-: Z [1] :=: EStrIndexOf x₂ (C c) (Z [i])
    | x₁ == x₂
    -> normRelA $ x₁ :=: S (rep Σ i ⋅ star (lit (neg c)) ⋅ lit c)
  -----------------------------------------------------------------------------
  --- |x|-[0,+∞] = str.indexof(x,c,i)   ≡   x = Σⁱc̄*(cΣ*)?
  EStrLen x₁ :-: Z (AIntFrom 0) :=: EStrIndexOf x₂ (C c) (Z [i])
    | x₁ == x₂
    -> normRelA $ x₁ :=: S (rep Σ i ⋅ star (lit (neg c)) ⋅ opt (lit c ⋅ star Σ))
  -----------------------------------------------------------------------------
  --- |x|-[j,+∞] = str.indexof(x,c,i)   ≡   x = Σⁱc̄*cΣ^(j-1)Σ*
  EStrLen x₁ :-: Z (AIntFrom j) :=: EStrIndexOf x₂ (C c) (Z [i])
    | x₁ == x₂, j >= 1
    -> normRelA $ x₁ :=: S (rep Σ i ⋅ star (lit (neg c)) ⋅ lit c ⋅ rep Σ (j-1) ⋅ star Σ)
  -----------------------------------------------------------------------------
  -- str.indexof(x,c,str.indexof(x,c̄,0)) = -1   ≡   x = c*c̄*
  EStrIndexOf x₁ (C c) (EStrIndexOf x₂ (C c̄) (Z [0])) :=: Z [-1]
    | x₁ == x₂, c̄ == neg c
    -> normRelA $ x₁ :=: S (star (lit c) ⋅ star (lit c̄))
  -----------------------------------------------------------------------------
  -- s[i..i] = c   ≡   s[i] = c
  EStrSub s i₁ i₂ :=: S t 
    | i₁ == i₂, Just c <- AString.toChar (t ∧ Σ) 
    -> normRelA $ EStrAt s i₁ :=: C c
  -----------------------------------------------------------------------------
  -- x[i..j] = s   ≡   x = Σⁱ(s ⊓ Σ^(j-i+1))Σ*
  EStrSub x (Z [i]) (Z [j]) :=: S s
    | i >= 0, i <= j, let s' = s ∧ rep Σ (j - i + 1)
    -> normRelA $ x :=: S (rep Σ i ⋅ s' ⋅ star Σ)
  -----------------------------------------------------------------------------
  -- x[i..|x|-1] = s   ≡   x = Σⁱs
  EStrSub x₁ (Z [i]) (EStrLen x₂ :-: Z [1]) :=: S s
    | x₁ == x₂, i >= 0
    -> normRelA $ x₁ :=: S (rep Σ i ⋅ s)
  -----------------------------------------------------------------------------
  -- x[i] = c   ≡   x = ΣⁱcΣ*
  EStrAt x (Z [i]) :=: C c
    | i >= 0 -> normRelA $ x :=: S (rep Σ i ⋅ lit c ⋅ star Σ)
  -----------------------------------------------------------------------------  
  EStrComp a :=: EStrComp b -> normRelA $ a :=: b
  EStrComp a :≠: EStrComp b -> normRelA $ a :≠: b
  EStrComp a :=: b          -> normRelA $ a :≠: b
  EStrComp a :≠: b          -> normRelA $ a :=: b
  a          :≠: EStrComp b -> normRelA $ a :=: b
  -----------------------------------------------------------------------------
  -- {i:ℤ | x[i+n] = a} = {j:ℤ | x[j+m] = b}
  ERelA  i₁ _ (EStrAt x₁ (i₂ :⨤: n) :=: C a) :=: 
   ERelA j₁ _ (EStrAt x₂ (j₂ :⨤: m) :=: C b)
    | i₁ == i₂, x₁ == x₂, j₁ == j₂
    , let k = m - n
    , let t | k > 0     = star Σ ⋅ lit a ⋅ rep Σ (k - 1) ⋅ lit b ⋅ star Σ
            | k < 0     = star Σ ⋅ lit b ⋅ rep Σ (k - 1) ⋅ lit a ⋅ star Σ
            | otherwise = star Σ ⋅ lit (a ∧ b) ⋅ star Σ
    -> normRelA $ x₁ :=: S t
  -----------------------------------------------------------------------------
  -- ⟨i: x[i-[2,∞]] = a]⟩ = ⟨j: x[j-[1,∞]] = b]⟩
  ERelA  i₁ _ (EStrAt x₁ (V i₂ :-: Z¹ (AIntFrom 2)) :=: C a) :=:
   ERelA j₁ _ (EStrAt x₂ (V j₂ :-: Z¹ (AIntFrom 1)) :=: C b)
    | i₁ == i₂, x₁ == x₂, j₁ == j₂
    , let t₁ = lit a ⋅ star Σ ⋅ lit b
    , let t₂ = lit b ⋅ star Σ ⋅ lit a ⋅ Σ
    , let t₃ = lit (a ∧ b) ⋅ Σ
    , let t = star Σ ⋅ (t₁ ∨ t₂ ∨ t₃) ⋅ star Σ
    -> normRelA $ x₁ :=: S t
  -----------------------------------------------------------------------------
  -- ⟨i: x[i-[1,∞]] = a]⟩ = ⟨j: x[j-[1,∞]] = b]⟩
  ERelA  i₁ _ (EStrAt x₁ (V i₂ :-: Z¹ (AIntFrom 1)) :=: C a) :=:
   ERelA j₁ _ (EStrAt x₂ (V j₂ :-: Z¹ (AIntFrom 1)) :=: C b)
    | i₁ == i₂, x₁ == x₂, j₁ == j₂
    , let t₁ = lit a ⋅ star Σ ⋅ lit b
    , let t₂ = lit b ⋅ star Σ ⋅ lit a
    , let t₃ = lit (a ∧ b)
    , let t = star Σ ⋅ (t₁ ∨ t₂ ∨ t₃) ⋅ star Σ
    -> normRelA $ x₁ :=: S t
  -----------------------------------------------------------------------------
  -- ⟨i: x[i-1] = a]⟩ = ⟨j: x[j-[1,∞] = b]⟩
  ERelA  i₁ _ (EStrAt x₁ (V i₂ :-: Z¹ AInt1       ) :=: C a) :=:
   ERelA j₁ _ (EStrAt x₂ (V j₂ :-: Z¹ (AIntFrom 1)) :=: C b)
    | i₁ == i₂, x₁ == x₂, j₁ == j₂
    , let t₂ = lit b ⋅ star Σ ⋅ lit a
    , let t₃ = lit (a ∧ b)
    , let t = star Σ ⋅ (t₂ ∨ t₃) ⋅ star Σ
    -> normRelA $ x₁ :=: S t
  -----------------------------------------------------------------------------
  -- ⟨i: x[i+[0,∞]] = a]⟩ = ⟨j: x[j-[0,∞] = b]⟩
  ERelA  i₁ _ (EStrAt x₁ (V i₂ :+: Z⁰ (AIntFrom 0)) :=: C a) :=:
   ERelA j₁ _ (EStrAt x₂ (V j₂ :-: Z⁰ (AIntFrom 0)) :=: C b)
    | i₁ == i₂, x₁ == x₂, j₁ == j₂
     , let t₁ = lit (a ∧ b)
     , let t₂ = lit a ⋅ star Σ ⋅ lit b
    , let t = star Σ ⋅ (t₁ ∨ t₂) ⋅ star Σ
    -> normRelA $ x₁ :=: S t
  -----------------------------------------------------------------------------
  -- [0,∞] = {x:ℤ | x % n = m}   ≡   ⊤   where n ≥ 0 and m ≥ 0
  Z (AIntFrom 0) :=: ERelA x₁ TInt (EMod (V x₂) (Z [n]) :=: Z [m])
    | x₁ == x₂, n >= 0, m >= 0
    -> Left True
  -----------------------------------------------------------------------------
  -- ω₁ = {x:ℤ | x ≠ ω₂}   ≡   ω₁ = ω₂ + [-∞,-1|1,∞]
  ω₁ :=: ERelA x TInt (V x₁ :≠: ω₂) 
    | x == x₁, x `notFreeIn` ω₂ 
    -> normRelA $ ω₁ :=: (ω₂ :+: Z (AInt.ne 0))
  -----------------------------------------------------------------------------
  ω :=: ERelA x _ ρ@(_ :=: _) | occurrences x ρ == 1 -> normRelA $ subst ω x ρ
  ω :=: ERelA x _ ρ           | concreteish ω        -> normRelA $ subst ω x ρ
  -----------------------------------------------------------------------------
  ρ -> Right ρ

-------------------------------------------------------------------------------

-- | Isolate a variable on the left-hand side of a relation, if possible.
isolate :: Name -> ARel -> ARel
isolate x ρ | occurrences x ρ /= 1 = ρ
isolate x ρ = flip rewrite ρ $ \case
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
 go = \case
  -----------------------------------------------------------------------------
  r | x `notFreeIn` r  -> ARel x τ r
  -----------------------------------------------------------------------------
  ω₁ :=: ω₂ | x `notFreeIn` ω₁ -> go $ ω₂ :=: ω₁
  ω₁ :≠: ω₂ | x `notFreeIn` ω₁ -> go $ ω₂ :≠: ω₁
  -----------------------------------------------------------------------------
  -- NOTE: below here, x occurs on the LHS and may also occur on the RHS
  -----------------------------------------------------------------------------
  -- str.indexof(x,c,0)+î = |x|
  (EStrIndexOf (V x₁) (C c) (Z [0]) :+: Z i) :=: EStrLen (V x₂)
    | x₁ == x₂ -> AString $ strWithFirstIndexOfCharRev c i
  -----------------------------------------------------------------------------
  -- ⟦str.indexof(x,a,0) + [0,∞] = str.indexof(x,b)⟧↑x  ≐  (ā ⊓ b̄)*((bā*)+(aΣ*))?
  (EStrIndexOf (V x₁) (C a) (Z [0]) :+: Z (AIntFrom 0)) :=: EStrIndexOf (V x₂) (C b) (Z [0])
    | x₁ == x₂, a /= b, let ā = neg a, let b̄ = neg b
    -> AString $ star (lit (ā ∧ b̄)) ⋅ opt ((lit b ⋅ star (lit ā)) ∨ (lit a ⋅ star Σ))
  -----------------------------------------------------------------------------
  -- ⟦str.indexof(x,a,0) - [-∞,1] = str.indexof(x,b)⟧↑x  ≐  (ā ⊓ b̄)*((bā*)+(aΣ*))?
  (EStrIndexOf (V x₁) (C a) (Z [0]) :-: Z (AIntTo 1)) :=: EStrIndexOf (V x₂) (C b) (Z [0])
    | x₁ == x₂, a /= b, let ā = neg a, let b̄ = neg b
    -> AString $ star (lit (ā ∧ b̄)) ⋅ opt ((lit b ⋅ star (lit ā)) ∨ (lit a ⋅ star Σ))
  -----------------------------------------------------------------------------
  ρ@(ω₁ :=: ω₂) | x `freeIn` ω₁, x `freeIn` ω₂ -> ARel x τ ρ
  ρ@(ω₁ :≠: ω₂) | x `freeIn` ω₁, x `freeIn` ω₂ -> ARel x τ ρ
  -----------------------------------------------------------------------------
  -- NOTE: below here, x occurs only on the LHS (possibly more than once)
  -----------------------------------------------------------------------------
  V _ :=: EVal â -> â
  -----------------------------------------------------------------------------  
  EVar _ TUnit   :≠: U â -> AUnit (neg â)
  EVar _ TBool   :≠: B â -> ABool (neg â)
  EVar _ TBool   :≠: ω   -> abstract x τ $ EVar x τ :=: ENot ω 
  EVar _ TInt    :≠: Z â -> AInt (neg â)
  EVar _ TChar   :≠: C â -> AChar (neg â)
  EVar _ TString :≠: S â -> AString (neg â)
  EVar _ TString :≠: ω   -> abstract x τ $ EVar x τ :=: EStrComp ω 
  -----------------------------------------------------------------------------
  EVar _ TString :=: EStrComp (S â) -> AString (neg â)
  -- NOTE: String complement is resolved here instead of during normalization,
  -- in order to exploit opportunities for double-negation elimination.
  -----------------------------------------------------------------------------
  (V _ :+: Z c) :=: ω -> abstract x τ $ EVar x τ :=: (ω :-: Z c)
  (V _ :-: Z c) :=: ω -> abstract x τ $ EVar x τ :=: (ω :+: Z c)
  -----------------------------------------------------------------------------
  EStrLen (V _) :=: Z n -> AString $ strOfLen n
  EStrLen (V _) :≠: Z n -> AString $ strNotOfLen n
  -----------------------------------------------------------------------------
  EStrAt (V _) (Z i) :=: C c -> AString $ strWithCharAt i c
  EStrAt (V _) (Z i) :≠: C c -> AString $ strWithoutCharAt i c
  -----------------------------------------------------------------------------
  EStrAt (V x₁) (EStrLen (V x₂) :-: Z i)   :=: C c | x₁ == x₂ -> AString $ strWithCharAtRev i c
  EStrAt (V x₁) (EStrLen (V x₂) :-: Z i)   :≠: C c | x₁ == x₂ -> AString $ strWithoutCharAtRev i c
  EStrAt (V x₁) (EStrLen (V x₂) :+: Z TOP) :=: C c | x₁ == x₂ -> AString $ strWithCharAtRev TOP c
  EStrAt (V x₁) (EStrLen (V x₂) :+: Z TOP) :≠: C c | x₁ == x₂ -> AString $ strWithoutCharAtRev TOP c
  -----------------------------------------------------------------------------
  EStrSub (V _) (Z i) (Z j) :=: S t -> AString $ strWithSubstr i j t
  EStrSub (V _) (Z i) (Z j) :≠: S t -> AString $ strWithoutSubstr i j t
  -----------------------------------------------------------------------------
  -- str.indexof(x,c,0) = i
  EStrIndexOf (V _) (C c) (Z [0]) :=: Z i -> AString $ strWithFirstIndexOfChar c i
  -----------------------------------------------------------------------------
  -- x[i..str.indexof(x,c,0)-j] = t   ≡   c̄ⁱ(t ⊓ c̄*)c̄^(j-1)cΣ*
  EStrSub (V x₁) (Z [i]) (EStrIndexOf (V x₂) (C c) (Z [0]) :-: Z [j]) :=: S t
    | x₁ == x₂, i >= 0, j >= 0, let c̄ = lit (neg c)
    -> AString $ rep c̄ i ⋅ (t ∧ star c̄) ⋅ rep c̄ (j-1) ⋅ lit c ⋅ star Σ      
  -----------------------------------------------------------------------------
  -- x[str.indexof(x,c,0)+i..|x|-j] = t
  EStrSub (V x₁) (EStrIndexOf (V x₂) (C c) (Z [0]) :+: Z [i]) (EStrLen (V x₃) :-: Z [j]) :=: S t
    | x₁ == x₂, x₂ == x₃ -> AString $ strWithSubstrFromFirstIndexOfCharToEnd c i j t
  -----------------------------------------------------------------------------
  -- str.indexof(x,c,0) = î
  EStrIndexOf (V _) (C c) (Z [0]) :=: Z i -> AString $ strWithFirstIndexOfChar c i
  -----------------------------------------------------------------------------
  -- str.indexof(x,a,str.indexof(x,b,0)+1) = î
  EStrIndexOf (V x₁) (C a) (EStrIndexOf (V x₂) (C b) (Z [0]) :+: Z [1]) :=: Z î
    | x₁ == x₂ -> AString $ strWithFirstIndexOfCharFollowedByFirstIndexOfChar b a î
  -----------------------------------------------------------------------------
  -- x[str.indexof(x,a,i)+n] = b   ≡   Σⁱā*aΣ^(n-1)bΣ*
  EStrAt (V x₁) (EStrIndexOf (V x₂) (C a) (Z [i]) :+: Z [n]) :=: C b
    | x₁ == x₂, let ā = lit (neg a)
    -> AString $ rep Σ i ⋅ star ā ⋅ lit a ⋅ rep Σ (n - 1) ⋅ lit b ⋅ star Σ
  -----------------------------------------------------------------------------
  EStrContains (V _) (S s) :=: B [doesContain]
    | doesContain -> AString t
    | otherwise   -> abstract x τ $ EVar x τ :=: EStrComp (S t)
   where
    t = star Σ ⋅ s ⋅ star Σ
  -----------------------------------------------------------------------------
  ρ -> ARel x τ (isolate x ρ)

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
