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
import Data.Text (Text)
import Data.Text qualified as Text
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
import Panini.Syntax
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
pattern 𝗨𝟭̂ :: AUnit -> AExpr
pattern 𝗨𝟭̂ a = EVal (AUnit a)

-- pattern U𝟙 :: AExpr
-- pattern U𝟙 = 𝗨𝟭̂ AUnit.Unit

-- | abstract Boolean constant
pattern 𝗕̂ :: ABool -> AExpr
pattern 𝗕̂ a = EVal (ABool a)

-- | abstract boolean constant representing a single concrete boolean
pattern 𝔹 :: Bool -> AExpr
pattern 𝔹 b <- 𝗕̂ (ABool.value -> Just b) where
  𝔹 b = 𝗕̂ (ABool.eq b)

-- | abstract integer constant
pattern 𝗭̂ :: AInt -> AExpr
pattern 𝗭̂ a = EVal (AInt a)

-- | abstract integer constant representing a single concrete integer
pattern ℤ :: Integer -> AExpr
pattern ℤ n <- 𝗭̂ (AInt.values -> [n]) where
  ℤ n = 𝗭̂ (AInt.eq n)

-- | abstract character constant
pattern 𝗖̂ :: AChar -> AExpr
pattern 𝗖̂ a = EVal (AChar a)

-- | abstract character constant representing a single concrete character
-- pattern ℂ𝕙 :: Char -> AExpr
-- pattern ℂ𝕙 c <- 𝗖̂ (AChar.values -> [c]) where
--   ℂ𝕙 c = 𝗖̂ (AChar.eq c)

-- | abstract string constant
pattern 𝗦̂ :: AString -> AExpr
pattern 𝗦̂ a = EVal (AString a)

-- | abstract string constant representing a one-character singleton string
pattern 𝗦̂1 :: AChar -> AExpr
pattern 𝗦̂1 a <- EVal (AString (AString.toChar -> Just a)) where
  𝗦̂1 a = EVal (AString (lit a))

-- | abstract string constant representing a single concrete string
pattern 𝕊 :: Text -> AExpr
pattern 𝕊 s <- 𝗦̂ (AString1 s) where
  𝕊 s = 𝗦̂ (AString.eq $ Text.unpack s)

-- | abstract integer addition
(⊕) :: AInt -> AInt -> AInt
(⊕) = AInt.add

-- | abstract integer subtraction
(⊖) :: AInt -> AInt -> AInt
(⊖) = AInt.sub

-- | abstract integer comparison with concrete integer
(⋖) :: AInt -> Integer -> Bool
(⋖) = AInt.isLe

-- | Match both x+n and x; in the latter case, n is taken to be 0.
pattern (:⨤:) :: Name -> Integer -> AExpr
pattern x :⨤: n <- (exprToVarPlusN -> Just (x,n))

exprToVarPlusN :: AExpr -> Maybe (Name, Integer)
exprToVarPlusN = \case
  EVar x TInt         -> Just (x, 0)
  EVar x TInt :+: ℤ n -> Just (x, n)
  _                   -> Nothing

-- | Matches any abstract integer but returns only its positive part, including
-- or excluding zero.
pattern 𝗭̂⁰, 𝗭̂¹ :: AInt -> AExpr
pattern 𝗭̂⁰ n̂ <- 𝗭̂ (meet (AInt.ge 0) -> n̂)
pattern 𝗭̂¹ n̂ <- 𝗭̂ (meet (AInt.ge 1) -> n̂)

pattern 𝕍 :: Name -> AExpr
pattern 𝕍 x <- EVar x _

-------------------------------------------------------------------------------

-- | Normalize an abstract expression by (partial) evaluation; see 'normRel'.
normExprA :: AExpr -> AExpr
normExprA = rewrite $ \case
  -----------------------------------------------------------------------------
  ERelA x₁ _ (isolate x₁ -> 𝕍 x₂ :=: ω) | x₁ == x₂, x₁ `notFreeIn` ω -> Just ω
  -----------------------------------------------------------------------------
  ERelA x b ρ -> case normRelA ρ of
    Left True            -> Just $ EAbs (topValue b)
    Left False           -> Just $ EAbs (botValue b)
    Right ρ' | ρ' /= ρ   -> Just $ ERelA x b ρ'
             | otherwise -> case abstract x b ρ of
                              ARel _ _ _ -> Nothing
                              a          -> Just $ EAbs a
  -----------------------------------------------------------------------------
  ENot (𝗕̂ a)    -> Just $ 𝗕̂ (neg a)
  ENot (ENot ω) -> Just ω
  -----------------------------------------------------------------------------
  𝗭̂ a :+: _    | isBot a -> Just $ 𝗭̂ a
  𝗭̂ a :-: _    | isBot a -> Just $ 𝗭̂ a
  _   :+: 𝗭̂ a  | isBot a -> Just $ 𝗭̂ a
  _   :-: 𝗭̂ a  | isBot a -> Just $ 𝗭̂ a
  𝗭̂ a :+: 𝗭̂ b            -> Just $ 𝗭̂ (a ⊕ b)
  𝗭̂ a :-: 𝗭̂ b            -> Just $ 𝗭̂ (a ⊖ b)
  ω   :+: ℤ 0            -> Just ω
  ω   :-: ℤ 0            -> Just ω
  ω   :+: 𝗭̂ a  | a ⋖ 0   -> Just $ ω :-: 𝗭̂ (AInt.negate a)
  ω   :-: 𝗭̂ a  | a ⋖ 0   -> Just $ ω :+: 𝗭̂ (AInt.negate a)
  -----------------------------------------------------------------------------
  (ω   :+: 𝗭̂ a) :+: 𝗭̂ b -> Just $ ω :+: 𝗭̂ (a ⊕ b)
  (ω   :+: 𝗭̂ a) :-: 𝗭̂ b -> Just $ ω :+: 𝗭̂ (a ⊖ b)
  (ω   :-: 𝗭̂ a) :+: 𝗭̂ b -> Just $ ω :-: 𝗭̂ (a ⊖ b)
  (ω   :-: 𝗭̂ a) :-: 𝗭̂ b -> Just $ ω :-: 𝗭̂ (a ⊕ b)
  (𝗭̂ a :+: ω  ) :+: 𝗭̂ b -> Just $ ω :+: 𝗭̂ (a ⊕ b)
  (𝗭̂ a :+: ω  ) :-: 𝗭̂ b -> Just $ ω :+: 𝗭̂ (a ⊖ b)
  (𝗭̂ a :-: ω  ) :+: 𝗭̂ b -> Just $ 𝗭̂ (a ⊕ b) :-: ω
  (𝗭̂ a :-: ω  ) :-: 𝗭̂ b -> Just $ 𝗭̂ (a ⊖ b) :-: ω
  -----------------------------------------------------------------------------
  EMod (ℤ a) (ℤ b) -> Just $ ℤ (a `mod` b)
  -----------------------------------------------------------------------------
  EStrLen (𝗦̂ s) | isTop s             -> Just $ 𝗭̂ (AInt.ge 0)
                | Just n <- strLen1 s -> Just $ ℤ n
  -- NOTE: We don't have any efficient way to compute nor represent, in general,
  -- the precise lengths of all strings contained in an abstract string.
  -----------------------------------------------------------------------------
  EStrAt (𝕊 s) (𝗭̂ i)                        -> Just $ 𝗖̂ (charAt s i)
  EStrAt (𝗦̂ s) (ℤ i)                        -> Just $ 𝗖̂ (charsAt s i)
  EStrAt (𝕍 x₁) (EStrLen (𝕍 x₂)) | x₁ == x₂ -> Just $ 𝗖̂ bot
  -----------------------------------------------------------------------------
  EStrAt s₁ (EStrLen s₂ :+: 𝗭̂ n) 
    | s₁ == s₂, let n' = n ∧ AInt.lt 0, n' /= n 
    -> Just $ EStrAt s₁ (EStrLen s₂ :-: 𝗭̂ (AInt.negate n'))
  -----------------------------------------------------------------------------
  EStrSub (𝕊 s) (𝗭̂ i) (𝗭̂ j)                           -> Just $ 𝗦̂ (strSub s i j)
  EStrSub (𝗦̂ s) (ℤ i₁) (ℤ i₂) | i₁ == i₂              -> Just $ 𝗦̂ (lit $ charsAt s i₁)
  EStrSub    ω₁ (ℤ 0) (EStrLen ω₂ :-: ℤ 1) | ω₁ == ω₂ -> Just ω₁
  -----------------------------------------------------------------------------
  EStrSub (EStrSub s (ℤ i) (ℤ j)) (ℤ k) (ℤ l)
    | i >= 0, i <= j, k >= 0, k <= l, l - k <= j - i
    -> Just $ EStrSub s (ℤ (i + k)) (ℤ (i + k + (l - k)))
  -----------------------------------------------------------------------------
  EStrComp (EStrComp ω) -> Just ω
  -- EStrComp (𝗦̂ s) -> Just $ 𝗦̂ (neg s)
  -- NOTE: We want to defer resolution of EStrComp as long as possible,
  -- in order to exploit opportunities for double-negation cancellation!
  -----------------------------------------------------------------------------
  EStrConc (𝗦̂ a) (𝗦̂ b) -> Just $ 𝗦̂ (a ⋅ b)
  EStrConc (EStrSub ω₁ (ℤ i₁) (ℤ j₁)) (EStrSub ω₂ (ℤ i₂) (ℤ j₂))
    | ω₁ == ω₂, i₁ <= j₁, j₁ + 1 == i₂, i₂ <= j₂
    -> Just $ EStrSub ω₁ (ℤ i₁) (ℤ j₂)
  -----------------------------------------------------------------------------
  EStrStar (𝗦̂ s) -> Just $ 𝗦̂ (star s)
  -----------------------------------------------------------------------------
  EStrContains (𝗦̂ ŝ₁) (𝗦̂ ŝ₂) -> Just $ 𝔹 (ŝ₂ ⊑ ŝ₁)
  -----------------------------------------------------------------------------
  _ -> Nothing

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
  ω₁ :≠: ℤ n -> normRelA $ ω₁ :=: 𝗭̂ (AInt.ne n)
  ω₁ :<: ℤ n -> normRelA $ ω₁ :=: 𝗭̂ (AInt.lt n)
  ω₁ :≤: ℤ n -> normRelA $ ω₁ :=: 𝗭̂ (AInt.le n)
  ω₁ :>: ℤ n -> normRelA $ ω₁ :=: 𝗭̂ (AInt.gt n)
  ω₁ :≥: ℤ n -> normRelA $ ω₁ :=: 𝗭̂ (AInt.ge n)
  ω₁ :<: ω₂  -> normRelA $ ω₁ :=: (ω₂ :-: 𝗭̂ (AInt.gt 0))
  ω₁ :≤: ω₂  -> normRelA $ ω₁ :=: (ω₂ :-: 𝗭̂ (AInt.ge 0))
  ω₁ :>: ω₂  -> normRelA $ ω₁ :=: (ω₂ :+: 𝗭̂ (AInt.gt 0))
  ω₁ :≥: ω₂  -> normRelA $ ω₁ :=: (ω₂ :+: 𝗭̂ (AInt.ge 0))
  -----------------------------------------------------------------------------
  -- NOTE: ">" is the structural ordering on 'AExpr'; after 
  -- this block, the "smaller" expression will be on the LHS,
  -- with variables < functions < constants
  ω₁ :=: ω₂ | ω₁ > ω₂ -> normRelA $ ω₂ :=: ω₁
  ω₁ :≠: ω₂ | ω₁ > ω₂ -> normRelA $ ω₂ :≠: ω₁
  -----------------------------------------------------------------------------
  EAbs â₁ :=: EAbs â₂ | Just â₃ <- â₁ ∧? â₂ -> Left (not $ hasBot â₃)
  EAbs â₁ :≠: EAbs â₂ | Just â₃ <- â₁ ∧? â₂ -> Left (hasBot â₃)
  -----------------------------------------------------------------------------
  ω :=: _ | anyBot ω -> Left False
  _ :=: ω | anyBot ω -> Left False
  ω :≠: _ | anyBot ω -> Left True
  _ :≠: ω | anyBot ω -> Left True
  -----------------------------------------------------------------------------
  ω₁           :=: ω₂           | ω₁ == ω₂ -> Left True
  ω₁           :=: (ω₂ :+: 𝗭̂ n̂) | ω₁ == ω₂ -> Left (AInt.member 0 n̂)
  ω₁           :=: (ω₂ :-: 𝗭̂ n̂) | ω₁ == ω₂ -> Left (AInt.member 0 n̂)
  (ω₁ :+: 𝗭̂ n̂) :=: ω₂           | ω₁ == ω₂ -> Left (AInt.member 0 n̂)
  (ω₁ :-: 𝗭̂ n̂) :=: ω₂           | ω₁ == ω₂ -> Left (AInt.member 0 n̂)
  -----------------------------------------------------------------------------
  ω₁           :≠: ω₂           | ω₁ == ω₂ -> Left False
  ω₁           :≠: (ω₂ :+: 𝗭̂ n̂) | ω₁ == ω₂ -> Left (not $ AInt.member 0 n̂)
  ω₁           :≠: (ω₂ :-: 𝗭̂ n̂) | ω₁ == ω₂ -> Left (not $ AInt.member 0 n̂)
  (ω₁ :+: 𝗭̂ n̂) :≠: ω₂           | ω₁ == ω₂ -> Left (not $ AInt.member 0 n̂)
  (ω₁ :-: 𝗭̂ n̂) :≠: ω₂           | ω₁ == ω₂ -> Left (not $ AInt.member 0 n̂)
  -----------------------------------------------------------------------------
  Rel o (𝗭̂ a :+: ω  ) (𝗭̂ b)         -> normRelA $ Rel o ω  (𝗭̂ (b ⊖ a))
  Rel o (𝗭̂ a :-: ω  ) (𝗭̂ b)         -> normRelA $ Rel o ω  (𝗭̂ (a ⊖ b))
  Rel o (ω   :+: 𝗭̂ a) (𝗭̂ b)         -> normRelA $ Rel o ω  (𝗭̂ (b ⊖ a))
  Rel o (ω   :-: 𝗭̂ a) (𝗭̂ b)         -> normRelA $ Rel o ω  (𝗭̂ (a ⊕ b))
  Rel o (𝗭̂ a :+: ω₁ ) (𝗭̂ b :+: ω₂ ) -> normRelA $ Rel o ω₁ (ω₂ :+: 𝗭̂ (b ⊖ a))
  Rel o (𝗭̂ a :-: ω₁ ) (𝗭̂ b :+: ω₂ ) -> normRelA $ Rel o ω₁ (𝗭̂ (a ⊖ b) :-: ω₂)
  Rel o (ω₁  :+: 𝗭̂ a) (𝗭̂ b :+: ω₂ ) -> normRelA $ Rel o ω₁ (ω₂ :+: 𝗭̂ (b ⊖ a))
  Rel o (ω₁  :-: 𝗭̂ a) (𝗭̂ b :+: ω₂ ) -> normRelA $ Rel o ω₁ (ω₂ :+: 𝗭̂ (a ⊕ b))
  Rel o (𝗭̂ a :+: ω₁ ) (𝗭̂ b :-: ω₂ ) -> normRelA $ Rel o ω₁ (𝗭̂ (b ⊖ a) :-: ω₂)
  Rel o (𝗭̂ a :-: ω₁ ) (𝗭̂ b :-: ω₂ ) -> normRelA $ Rel o ω₁ (ω₂ :+: 𝗭̂ (a ⊖ b))
  Rel o (ω₁  :+: 𝗭̂ a) (𝗭̂ b :-: ω₂ ) -> normRelA $ Rel o ω₁ (𝗭̂ (b ⊖ a) :-: ω₂)
  Rel o (ω₁  :-: 𝗭̂ a) (𝗭̂ b :-: ω₂ ) -> normRelA $ Rel o ω₁ (𝗭̂ (a ⊕ b) :-: ω₂)
  Rel o (𝗭̂ a :+: ω₁ ) (ω₂  :+: 𝗭̂ b) -> normRelA $ Rel o ω₁ (ω₂ :+: 𝗭̂ (b ⊖ a))
  Rel o (𝗭̂ a :-: ω₁ ) (ω₂  :+: 𝗭̂ b) -> normRelA $ Rel o ω₁ (𝗭̂ (a ⊖ b) :-: ω₂)
  Rel o (ω₁  :+: 𝗭̂ a) (ω₂  :+: 𝗭̂ b) -> normRelA $ Rel o ω₁ (ω₂ :+: 𝗭̂ (b ⊖ a))
  Rel o (ω₁  :-: 𝗭̂ a) (ω₂  :+: 𝗭̂ b) -> normRelA $ Rel o ω₁ (ω₂ :+: 𝗭̂ (a ⊕ b))
  Rel o (𝗭̂ a :+: ω₁ ) (ω₂  :-: 𝗭̂ b) -> normRelA $ Rel o ω₁ (ω₂ :-: 𝗭̂ (a ⊕ b))
  Rel o (𝗭̂ a :-: ω₁ ) (ω₂  :-: 𝗭̂ b) -> normRelA $ Rel o ω₁ (𝗭̂ (a ⊕ b) :-: ω₂)
  Rel o (ω₁  :+: 𝗭̂ a) (ω₂  :-: 𝗭̂ b) -> normRelA $ Rel o ω₁ (ω₂ :-: 𝗭̂ (a ⊕ b))
  Rel o (ω₁  :-: 𝗭̂ a) (ω₂  :-: 𝗭̂ b) -> normRelA $ Rel o ω₁ (ω₂ :+: 𝗭̂ (a ⊖ b))
  -----------------------------------------------------------------------------
  EMod (𝗭̂ â) (ℤ b) :=: ℤ c 
    | any (\x -> x `mod` b == c) $ take 100 $ AInt.values â
    -> Left True
  -----------------------------------------------------------------------------
  𝗭̂ î :=: ERelA x₁ TInt (EMod (EVar x₂ _) (ℤ n) :=: ℤ m)
    | x₁ == x₂, n >= 0, m >= 0, î ∧ AInt.ge 0 == AInt.ge 0 
    -> Left True
  -----------------------------------------------------------------------------
  EStrIndexOf s c i :≠: 𝗭̂ n̂ -> normRelA $ EStrIndexOf s c i :=: 𝗭̂ (neg n̂)
  EStrIndexOf s (𝗦̂1 ĉ) (ℤ 0) :=: 𝗭̂ n̂ 
    -> normRelA $ s :=: 𝗦̂ (strWithFirstIndexOfChar ĉ (n̂ ∧ AInt.ge (-1)))
  EStrIndexOf s t i :=: 𝗭̂ n 
    | n == AInt.lt 0
    -> normRelA $ EStrIndexOf s t i :=: ℤ (-1)
  -----------------------------------------------------------------------------
  -- str.indexof(x,ω,|x|+[-i,+∞]) = ψ   ≡   str.indexof(x,ω,|x|-[0,i]) = ψ
  EStrIndexOf x₁ ω (EStrLen x₂ :+: 𝗭̂ n̂) :=: ψ
    | x₁ == x₂
    , [Fin i :… PosInf] <- AInt.intervals n̂, i < 0
    , let n̂' = AInt.fromTo 0 (Prelude.negate i)
    -> normRelA $ EStrIndexOf x₁ ω (EStrLen x₂ :-: 𝗭̂ n̂') :=: ψ
  -----------------------------------------------------------------------------
  -- str.indexof(x,c,[i,+∞]) = -1   ≡   x = ΣⁱΣ*c̄*
  EStrIndexOf x (𝗦̂1 c) (𝗭̂ (AIntFrom i)) :=: ℤ (-1) 
    -> normRelA $ x :=: 𝗦̂ (rep Σ i ⋅ star Σ ⋅ star (lit (neg c)))
  -----------------------------------------------------------------------------
  -- str.indexof(x,c,|x|-[i,+∞]) = -1   ≡   x = Σ*c̄ⁱc̄*
  EStrIndexOf x₁ (𝗦̂1 c) (EStrLen x₂ :-: 𝗭̂ (AIntFrom i)) :=: ℤ (-1)
    | x₁ == x₂, let c̄ = lit (neg c)
    -> normRelA $ x₁ :=: 𝗦̂ (star Σ ⋅ rep c̄ i ⋅ star c̄)
  -----------------------------------------------------------------------------
  -- TODO: generalize
  -- str.indexof(x,c,|x|-[0,1]) = -1   ≡   x = Σ*c̄?
  EStrIndexOf x₁ (𝗦̂1 c) (EStrLen x₂ :-: 𝗭̂ n̂) :=: ℤ (-1)
    | x₁ == x₂
    , [Fin 0 :… Fin 1] <- AInt.intervals n̂
    -> normRelA $ x₁ :=: 𝗦̂ (star Σ ⋅ opt (lit (neg c)))
  -----------------------------------------------------------------------------
  i₁ :=: EStrIndexOf s t i₂ 
    | i₁ == i₂ 
    -> normRelA $ EStrSub s i₂ (i₂ :+: (EStrLen t :-: ℤ 1)) :=: t
  i₁ :+: 𝗭̂ n̂ :=: EStrIndexOf s t i₂ 
    | i₁ == i₂, let n̂' = n̂ ∧ AInt.ge 0, n̂' /= n̂ 
    -> normRelA $ i₁ :+: 𝗭̂ n̂' :=: EStrIndexOf s t i₂
  i₁ :-: 𝗭̂ n̂ :=: EStrIndexOf s t i₂ 
    | i₁ == i₂, let n̂' = n̂ ∧ AInt.le 0, n̂' /= n̂ 
    -> normRelA $ i₁ :-: 𝗭̂ n̂' :=: EStrIndexOf s t i₂
  EStrLen s₁ :+: 𝗭̂ n̂ :=: EStrIndexOf s₂ t i 
    | s₁ == s₂, let n̂' = n̂ ∧ AInt.lt 0, n̂' /= n̂ 
    -> normRelA $ EStrLen s₁ :+: 𝗭̂ n̂' :=: EStrIndexOf s₂ t i
  i₁ :≠: EStrIndexOf s (𝗦̂1 c) i₂ 
    | i₁ == i₂
    -> normRelA $ i₁ :=: EStrIndexOf s (𝗦̂1 (neg c)) (ℤ 0)
  -----------------------------------------------------------------------------
  --- |x|-1 = str.indexof(x,c,i)   ≡   x = Σⁱc̄*c
  EStrLen x₁ :-: ℤ 1 :=: EStrIndexOf x₂ (𝗦̂1 c) (ℤ i)
    | x₁ == x₂
    -> normRelA $ x₁ :=: 𝗦̂ (rep Σ i ⋅ star (lit (neg c)) ⋅ lit c)
  -----------------------------------------------------------------------------
  --- |x|-[0,+∞] = str.indexof(x,c,i)   ≡   x = Σⁱc̄*(cΣ*)?
  EStrLen x₁ :-: 𝗭̂ (AIntFrom 0) :=: EStrIndexOf x₂ (𝗦̂1 c) (ℤ i)
    | x₁ == x₂
    -> normRelA $ x₁ :=: 𝗦̂ (rep Σ i ⋅ star (lit (neg c)) ⋅ opt (lit c ⋅ star Σ))
  -----------------------------------------------------------------------------
  --- |x|-[j,+∞] = str.indexof(x,c,i)   ≡   x = Σⁱc̄*cΣ^(j-1)Σ*
  EStrLen x₁ :-: 𝗭̂ (AIntFrom j) :=: EStrIndexOf x₂ (𝗦̂1 c) (ℤ i)
    | x₁ == x₂, j >= 1
    -> normRelA $ x₁ :=: 𝗦̂ (rep Σ i ⋅ star (lit (neg c)) ⋅ lit c ⋅ rep Σ (j-1) ⋅ star Σ)
  -----------------------------------------------------------------------------
  -- str.indexof(x,c,str.indexof(x,c̄,0)) = -1   ≡   x = c*c̄*
  EStrIndexOf x₁ (𝗦̂1 c) (EStrIndexOf x₂ (𝗦̂1 c̄) (ℤ 0)) :=: ℤ (-1)
    | x₁ == x₂, c̄ == neg c
    -> normRelA $ x₁ :=: 𝗦̂ (star (lit c) ⋅ star (lit c̄))
  -----------------------------------------------------------------------------
  -- s[i..i] = c   ≡   s[i] = c
  EStrSub s i₁ i₂ :=: 𝗦̂ t 
    | i₁ == i₂, Just c <- AString.toChar (t ∧ Σ) 
    -> normRelA $ EStrAt s i₁ :=: 𝗖̂ c
  -----------------------------------------------------------------------------
  -- x[i..j] = s   ≡   x = Σⁱ(s ⊓ Σ^(j-i+1))Σ*
  EStrSub x (ℤ i) (ℤ j) :=: 𝗦̂ s
    | i >= 0, i <= j, let s' = s ∧ rep Σ (j - i + 1)
    -> normRelA $ x :=: 𝗦̂ (rep Σ i ⋅ s' ⋅ star Σ)
  -----------------------------------------------------------------------------
  -- x[i..|x|-1] = s   ≡   x = Σⁱs
  EStrSub x₁ (ℤ i) (EStrLen x₂ :-: ℤ 1) :=: 𝗦̂ s
    | x₁ == x₂, i >= 0
    -> normRelA $ x₁ :=: 𝗦̂ (rep Σ i ⋅ s)
  -----------------------------------------------------------------------------
  -- x[i] = c   ≡   x = ΣⁱcΣ*
  EStrAt x (ℤ i) :=: 𝗖̂ c
    | i >= 0 -> normRelA $ x :=: 𝗦̂ (rep Σ i ⋅ lit c ⋅ star Σ)
  -----------------------------------------------------------------------------
  EStrComp a :=: EStrComp b -> normRelA $ a :=: b
  EStrComp a :≠: EStrComp b -> normRelA $ a :≠: b
  EStrComp a :=: b          -> normRelA $ a :≠: b
  EStrComp a :≠: b          -> normRelA $ a :=: b
  a          :≠: EStrComp b -> normRelA $ a :=: b
  -----------------------------------------------------------------------------
  -- ⟨i: x[i+n] = a⟩ = ⟨j: x[j+m] = b⟩
  ERelA  i₁ _ (EStrAt x₁ (i₂ :⨤: n) :=: 𝗖̂ a) :=: 
   ERelA j₁ _ (EStrAt x₂ (j₂ :⨤: m) :=: 𝗖̂ b)
    | i₁ == i₂, x₁ == x₂, j₁ == j₂
    , let k = m - n
    , let t | k > 0     = star Σ ⋅ lit a ⋅ rep Σ (k - 1) ⋅ lit b ⋅ star Σ
            | k < 0     = star Σ ⋅ lit b ⋅ rep Σ (k - 1) ⋅ lit a ⋅ star Σ
            | otherwise = star Σ ⋅ lit (a ∧ b) ⋅ star Σ
    -> normRelA $ x₁ :=: 𝗦̂ t
  -----------------------------------------------------------------------------
  -- ⟨i: x[i-[2,∞]] = a]⟩ = ⟨j: x[j-[1,∞]] = b]⟩
  ERelA  i₁ _ (EStrAt x₁ (𝕍 i₂ :-: 𝗭̂¹ (AIntFrom 2)) :=: 𝗖̂ a) :=:
   ERelA j₁ _ (EStrAt x₂ (𝕍 j₂ :-: 𝗭̂¹ (AIntFrom 1)) :=: 𝗖̂ b)
    | i₁ == i₂, x₁ == x₂, j₁ == j₂
    , let t₁ = lit a ⋅ star Σ ⋅ lit b
    , let t₂ = lit b ⋅ star Σ ⋅ lit a ⋅ Σ
    , let t₃ = lit (a ∧ b) ⋅ Σ
    , let t = star Σ ⋅ (t₁ ∨ t₂ ∨ t₃) ⋅ star Σ
    -> normRelA $ x₁ :=: 𝗦̂ t
  -----------------------------------------------------------------------------
  -- ⟨i: x[i-[1,∞]] = a]⟩ = ⟨j: x[j-[1,∞]] = b]⟩
  ERelA  i₁ _ (EStrAt x₁ (𝕍 i₂ :-: 𝗭̂¹ (AIntFrom 1)) :=: 𝗖̂ a) :=:
   ERelA j₁ _ (EStrAt x₂ (𝕍 j₂ :-: 𝗭̂¹ (AIntFrom 1)) :=: 𝗖̂ b)
    | i₁ == i₂, x₁ == x₂, j₁ == j₂
    , let t₁ = lit a ⋅ star Σ ⋅ lit b
    , let t₂ = lit b ⋅ star Σ ⋅ lit a
    , let t₃ = lit (a ∧ b)
    , let t = star Σ ⋅ (t₁ ∨ t₂ ∨ t₃) ⋅ star Σ
    -> normRelA $ x₁ :=: 𝗦̂ t
  -----------------------------------------------------------------------------
  -- ⟨i: x[i-1] = a]⟩ = ⟨j: x[j-[1,∞] = b]⟩
  ERelA  i₁ _ (EStrAt x₁ (𝕍 i₂ :-: 𝗭̂¹ AInt1       ) :=: 𝗖̂ a) :=:
   ERelA j₁ _ (EStrAt x₂ (𝕍 j₂ :-: 𝗭̂¹ (AIntFrom 1)) :=: 𝗖̂ b)
    | i₁ == i₂, x₁ == x₂, j₁ == j₂
    , let t₂ = lit b ⋅ star Σ ⋅ lit a
    , let t₃ = lit (a ∧ b)
    , let t = star Σ ⋅ (t₂ ∨ t₃) ⋅ star Σ
    -> normRelA $ x₁ :=: 𝗦̂ t
  -----------------------------------------------------------------------------
  -- ⟨i: x[i+[0,∞]] = a]⟩ = ⟨j: x[j-[0,∞] = b]⟩
  ERelA  i₁ _ (EStrAt x₁ (𝕍 i₂ :+: 𝗭̂⁰ (AIntFrom 0)) :=: 𝗖̂ a) :=:
   ERelA j₁ _ (EStrAt x₂ (𝕍 j₂ :-: 𝗭̂⁰ (AIntFrom 0)) :=: 𝗖̂ b)
    | i₁ == i₂, x₁ == x₂, j₁ == j₂
     , let t₁ = lit (a ∧ b)
     , let t₂ = lit a ⋅ star Σ ⋅ lit b
    , let t = star Σ ⋅ (t₁ ∨ t₂) ⋅ star Σ
    -> normRelA $ x₁ :=: 𝗦̂ t
  -----------------------------------------------------------------------------
  -- ω₁ = {x:ℤ | x ≠ ω₂}   ≡   ω₁ = ω₂ + [-∞,-1|1,∞]
  ω₁ :=: ERelA x TInt (𝕍 x₁ :≠: ω₂) 
    | x == x₁, x `notFreeIn` ω₂ 
    -> normRelA $ ω₁ :=: (ω₂ :+: 𝗭̂ (AInt.ne 0))
  -----------------------------------------------------------------------------
  ω :=: ERelA x _ ρ@(_ :=: _) | occurrences x ρ == 1 -> normRelA $ subst ω x ρ
  ω :=: ERelA x _ ρ           | concreteish ω        -> normRelA $ subst ω x ρ
  -----------------------------------------------------------------------------
  ρ -> Right ρ

-------------------------------------------------------------------------------

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
  ρ | x `notFreeIn` ρ  -> ARel x τ ρ  -- TODO: topValue b ??
  -----------------------------------------------------------------------------
  ω₁ :=: ω₂ | x `notFreeIn` ω₁ -> go $ ω₂ :=: ω₁
  ω₁ :≠: ω₂ | x `notFreeIn` ω₁ -> go $ ω₂ :≠: ω₁
  -----------------------------------------------------------------------------
  -- NOTE: below here, x occurs on the LHS and may also occur on the RHS
  -----------------------------------------------------------------------------
  (EStrFirstIndexOfChar (𝕍 x₁) (𝗖̂ c) :+: 𝗭̂ i) :=: EStrLen (𝕍 x₂)
    | x₁ == x₂ -> AString $ strWithFirstIndexOfCharRev c i
  -----------------------------------------------------------------------------
  (EStrFirstIndexOfChar (𝕍 x₁) (𝗖̂ a) :+: 𝗭̂ (AIntFrom 0)) :=: EStrFirstIndexOfChar (𝕍 x₂) (𝗖̂ b)
    | x₁ == x₂, a /= b, let ā = neg a, let b̄ = neg b
    -> AString $ star (lit (ā ∧ b̄)) ⋅ opt ((lit b ⋅ star (lit ā)) ∨ (lit a ⋅ star Σ))
  -----------------------------------------------------------------------------
  (EStrFirstIndexOfChar (𝕍 x₁) (𝗖̂ a) :-: 𝗭̂ (AIntTo 1)) :=: EStrFirstIndexOfChar (𝕍 x₂) (𝗖̂ b)
    | x₁ == x₂, a /= b, let ā = neg a, let b̄ = neg b
    -> AString $ star (lit (ā ∧ b̄)) ⋅ opt ((lit b ⋅ star (lit ā)) ∨ (lit a ⋅ star Σ))
  -----------------------------------------------------------------------------
  ρ@(ω₁ :=: ω₂) | x `freeIn` ω₁, x `freeIn` ω₂ -> ARel x τ ρ
  ρ@(ω₁ :≠: ω₂) | x `freeIn` ω₁, x `freeIn` ω₂ -> ARel x τ ρ
  -----------------------------------------------------------------------------
  -- NOTE: below here, x occurs only on the LHS (possibly more than once)
  -----------------------------------------------------------------------------
  𝕍 _ :=: EVal â -> â
  -----------------------------------------------------------------------------  
  EVar _ TUnit   :≠: 𝗨𝟭̂ â -> AUnit (neg â)
  EVar _ TBool   :≠: 𝗕̂ â  -> ABool (neg â)
  EVar _ TBool   :≠: ω    -> abstract x τ $ EVar x τ :=: ENot ω 
  EVar _ TInt    :≠: 𝗭̂ â  -> AInt (neg â)
  EVar _ TChar   :≠: 𝗖̂ â  -> AChar (neg â)
  EVar _ TString :≠: 𝗦̂ â  -> AString (neg â)
  EVar _ TString :≠: ω    -> abstract x τ $ EVar x τ :=: EStrComp ω 
  -----------------------------------------------------------------------------
  EVar _ TString :=: EStrComp (𝗦̂ â) -> AString (neg â)
  -- NOTE: String complement is resolved here instead of during normalization,
  -- in order to exploit opportunities for double-negation elimination.
  -----------------------------------------------------------------------------
  (𝕍 _ :+: 𝗭̂ c) :=: ω -> abstract x τ $ EVar x τ :=: (ω :-: 𝗭̂ c)
  (𝕍 _ :-: 𝗭̂ c) :=: ω -> abstract x τ $ EVar x τ :=: (ω :+: 𝗭̂ c)
  -----------------------------------------------------------------------------
  EStrLen (𝕍 _) :=: 𝗭̂ n -> AString $ strOfLen n
  EStrLen (𝕍 _) :≠: 𝗭̂ n -> AString $ strNotOfLen n
  -----------------------------------------------------------------------------
  EStrAt (𝕍 _) (𝗭̂ i) :=: 𝗖̂ c -> AString $ strWithCharAt i c
  EStrAt (𝕍 _) (𝗭̂ i) :≠: 𝗖̂ c -> AString $ strWithoutCharAt i c
  -----------------------------------------------------------------------------
  EStrAt (𝕍 x₁) (EStrLen (𝕍 x₂) :-: 𝗭̂ i)   :=: 𝗖̂ c | x₁ == x₂ -> AString $ strWithCharAtRev i c
  EStrAt (𝕍 x₁) (EStrLen (𝕍 x₂) :-: 𝗭̂ i)   :≠: 𝗖̂ c | x₁ == x₂ -> AString $ strWithoutCharAtRev i c
  EStrAt (𝕍 x₁) (EStrLen (𝕍 x₂) :+: 𝗭̂ TOP) :=: 𝗖̂ c | x₁ == x₂ -> AString $ strWithCharAtRev TOP c
  EStrAt (𝕍 x₁) (EStrLen (𝕍 x₂) :+: 𝗭̂ TOP) :≠: 𝗖̂ c | x₁ == x₂ -> AString $ strWithoutCharAtRev TOP c
  -----------------------------------------------------------------------------
  EStrSub (𝕍 _) (𝗭̂ i) (𝗭̂ j) :=: 𝗦̂ t -> AString $ strWithSubstr i j t
  EStrSub (𝕍 _) (𝗭̂ i) (𝗭̂ j) :≠: 𝗦̂ t -> AString $ strWithoutSubstr i j t
  -----------------------------------------------------------------------------
  EStrFirstIndexOfChar (𝕍 _) (𝗖̂ c) :=: 𝗭̂ i -> AString $ strWithFirstIndexOfChar c i
  -----------------------------------------------------------------------------
  EStrSub (𝕍 x₁) (ℤ i) (EStrFirstIndexOfChar (𝕍 x₂) (𝗖̂ c) :-: ℤ j) :=: 𝗦̂ t
    | x₁ == x₂, i >= 0, j >= 0, let c̄ = lit (neg c)
    -> AString $ rep c̄ i ⋅ (t ∧ star c̄) ⋅ rep c̄ (j-1) ⋅ lit c ⋅ star Σ      
  -----------------------------------------------------------------------------
  EStrSub (𝕍 x₁) (EStrFirstIndexOfChar (𝕍 x₂) (𝗖̂ c) :+: ℤ i) (EStrLen (𝕍 x₃) :-: ℤ j) :=: 𝗦̂ t
    | x₁ == x₂, x₂ == x₃ -> AString $ strWithSubstrFromFirstIndexOfCharToEnd c i j t
  -----------------------------------------------------------------------------
  EStrIndexOf (𝕍 _) (𝗦̂1 c) (𝗭̂ AInt0) :=: 𝗭̂ i -> AString $ strWithFirstIndexOfChar c i
  -----------------------------------------------------------------------------
  EStrIndexOf (𝕍 x₁) (𝗦̂1 c₁) (EStrIndexOf (𝕍 x₂) (𝗦̂1 c₂) (𝗭̂ AInt0) :+: 𝗭̂ AInt1) :=: 𝗭̂ k
    | x₁ == x₂ -> AString $ strWithFirstIndexOfCharFollowedByFirstIndexOfChar c₂ c₁ k
  -----------------------------------------------------------------------------
  EStrAt (𝕍 x₁) (EStrIndexOf (𝕍 x₂) (𝗦̂1 c₁) (ℤ i) :+: ℤ n) :=: 𝗖̂ c₂
    | x₁ == x₂, let c̄₁ = lit (neg c₁)
    -> AString $ rep Σ i ⋅ star c̄₁ ⋅ lit c₁ ⋅ rep Σ (n - 1) ⋅ lit c₂ ⋅ star Σ
  -----------------------------------------------------------------------------
  EStrContains (𝕍 _) (𝗦̂ s) :=: 𝔹 doesContain
    | doesContain -> AString t
    | otherwise   -> abstract x τ $ EVar x τ :=: EStrComp (𝗦̂ t)
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
