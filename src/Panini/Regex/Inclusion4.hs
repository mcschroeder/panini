{-# OPTIONS_GHC -Wno-operator-whitespace #-}

{-
This module implements fast regular expression inclusion testing for certain
kinds of expressions.

References:

  * Hovland, Dag. 2012. "The inclusion problem for regular expressions." 
    Journal of Computer and System Sciences 78 (2012): 1795-1813.
    https://doi.org/10.1016/j.jcss.2011.12.003

-}
module Panini.Regex.Inclusion4 (isIncludedBy4) where

import Control.Exception
import Data.Foldable
import Data.Maybe
import Data.Set qualified as Set
import Panini.Regex.CharSet (CharSet)
import Panini.Regex.CharSet qualified as CS
import Panini.Regex.Type
import Prelude
import Panini.Pretty
import Control.Monad.ST
import Data.STRef
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)
import Panini.Pretty

import Debug.Trace
--trace _ = id


-------------------------------------------------------------------------------

-- | The set of letters that can occur first in a word in the language, i.e.,
-- first(r) = {l ∈ Σ ∣ ∃w: l⋅w ∈ L(r)} (Hovland 2012, Table 1).
first :: Regex -> CharSet
first = \case
  One             -> mempty
  Lit p           -> p
  Times1 r1 r2 
    | nullable r1 -> first r1 <> first r2
    | otherwise   -> first r1
  Plus1 r1 r2     -> first r1 <> first r2
  Star r          -> first r
  Opt r           -> first r

-------------------------------------------------------------------------------

-- We will use some notational conveniences throughout this module.

(∈) :: Eq a => Foldable t => a -> t a -> Bool
(∈) = elem

(⊆) :: CharSet -> CharSet -> Bool
(⊆) = CS.isSubsetOf

(∪), (∩) :: CharSet -> CharSet -> CharSet
(∪) = CS.union
(∩) = CS.intersection

-- | Is the intersection between two character sets non-empty?
(∩?) :: CharSet -> CharSet -> Bool
a ∩? b = not $ CS.null $ a ∩ b
infix 4 ∩?

-- | The intersection of a character set and the first-set of a regex.
(⩀) :: CharSet -> Regex -> CharSet
c ⩀ r = c `CS.intersection` first r
infix 4 ⩀

-- | Is the first-set of a regex contained in the first-set of another?
(⋖) :: Regex -> Regex -> Bool
r₁ ⋖ r₂ = first r₁ ⊆ first r₂
infix 4 ⋖

-- | Is the intersection between the first-sets of two regexes non-empty? 
(⊙) :: Regex -> Regex -> Bool
r₁ ⊙ r₂ = not $ CS.null $ first r₁ `CS.intersection` first r₂
infix 4 ⊙

𝔫, 𝔫̸ :: Regex -> Bool
𝔫 = nullable
𝔫̸ = not . nullable

(⋅) :: Regex -> Regex -> Regex
(⋅) = times
infix 5 ⋅

isLit :: Regex -> Bool
isLit = \case
  Lit _ -> True
  _     -> False

isStar :: Regex -> Bool
isStar = \case
  Star _ -> True
  _      -> False

-- | A special version of the 'Times1' pattern that matches not only r₁⋅r₂
-- (where r₂ ≠ ε) but also r₁⋅ε (which is normally just r₁) by introducing
-- a spurious empty word. Note that in both cases r₁ ≠ ε.
pattern (:⋅) :: Regex -> Regex -> Regex
pattern r₁ :⋅ r₂ <- (unconsTimes' -> Just (r₁,r₂))
{-# INLINE (:⋅) #-}

unconsTimes' :: Regex -> Maybe (Regex, Regex)
unconsTimes' = \case
  Zero         -> Nothing
  One          -> Nothing
  Times1 r₁ r₂ -> Just (r₁, r₂)
  r            -> Just (r, One)
{-# INLINE unconsTimes' #-}

-- | A special version of the 'Plus1' pattern that matches (r₁ + r₂) and also
-- deconstructs optionals r? into choices (r + ε).
pattern (:+) :: Regex -> Regex -> Regex
pattern r₁ :+ r₂ <- (unconsPlus' -> Just (r₁,r₂))
{-# INLINE (:+) #-}

unconsPlus' :: Regex -> Maybe (Regex, Regex)
unconsPlus' = \case
  Plus1 r₁ r₂ -> Just (r₁, r₂)
  Opt r       -> Just (r, One)
  _           -> Nothing
{-# INLINE unconsPlus' #-}

-- | The '⊑' type is a reified regular expression inclusion relation.
data a ⊑ b = a :⊑ b 
  deriving stock (Eq, Ord, Show, Read)

infix 0 :⊑

instance (Pretty a, Pretty b) => Pretty (a ⊑ b) where
  pretty (a :⊑ b) = pretty a <+> "⊑" <+> pretty b

instance Pretty Bool where
  pretty = pretty . show
-------------------------------------------------------------------------------

-- all premises of any branch

isIncludedBy4 :: Regex -> Regex -> Bool
isIncludedBy4 r₁ r₂ = go mempty [(mempty,[r₁ :⊑ r₂])]
 where
  go f bs = trace (showPretty $ pretty (map snd bs) <\\> "-----") go' f bs
  -- go f bs = go' f bs
  go' _ [] = trace "FALSE" False
  go' _ ((_,[]):bs) = trace "TRUE" True
  go' f ((s,c:cs):bs)
    | c ∈ s = trace "DROP" go f ((s,cs):bs)
    | c ∈ f = trace "ELIM" go f bs
    | otherwise = trace "MATCH" $ case match c of
        [] -> go (Set.insert c f) bs
        ps -> go f (map (\p -> (s', p ++ cs)) ps ++ bs) where s' = Set.insert c s


-- trace' s f bs r = trace $ showPretty $
--   "s:" <+> pretty s <\\>
--   "f:" <+> pretty f <\\>
--   "bs:" <+> pretty bs <\\>
--   "-->" <+> pretty r <\\>
--   "------------------------------"

(∖) :: CharSet -> CharSet -> CharSet
a ∖ b = a ∩ (CS.complement b)

match :: (Regex ⊑ Regex) -> [[Regex ⊑ Regex]]
match conclusion@(rᴸ :⊑ rᴿ) = case conclusion of
  -- Axm ----------------------------------------------------------------------
  One :⊑ r 
    | 𝔫 r                             -> trace "Axm" [[]]

  -- LitSplit
  Lit l₁ :⋅ r₃ :⊑ r₄
    | CS.size l₁ > 1 -> trace "LitSplit" [map (\c -> (Lit (CS.singleton c))⋅r₃ :⊑ r₄) (CS.toList l₁)]

  -- Letter -------------------------------------------------------------------
  Lit l₁ :⋅ r₁ :⊑ Lit l₂  :⋅ r₂ 
    | l₁ ⊆ l₂                         -> trace "Letter" [[r₁ :⊑ r₂]]

  -- LetterStar + ElimCat ----------------------------------------
  Lit l :⋅ r₁ :⊑ Star r₂ :⋅ r₃
    | letterStar, elimCat -> trace "LetterStar_1 **" [[rᴸ :⊑ r₂⋅rᴿ],[rᴸ :⊑ r₃]]
    | letterStar          -> trace "LetterStar_2" [[rᴸ :⊑ r₂⋅rᴿ]]
    | elimCat             -> trace "LetterStar_3" [[rᴸ :⊑ r₃]]
   where
    letterStar = l ⊆ first r₂
    elimCat    = l ⊆ first r₃

  -- -- LetterStar + ElimCat + LeftChoice ----------------------------------------
  -- Lit l :⋅ r₁ :⊑ Star r₂ :⋅ r₃
  --   | l₂ ∪ l₃ /= l                    -> trace "LetterStar_1" []             -- no match
  --   | l₂ ∩? l₃                        -> trace "LetterStar_2 **" mkBranch pamb   -- ambiguous
  --   | otherwise                       -> trace "LetterStar_3" [p₂ ++ p₃]
  --  where
  --   l₂ = l ⩀ r₂
  --   l₃ = l ⩀ r₃
  --   p₂ = if CS.null l₂ then [] else [(Lit l₂)⋅r₁ :⊑ r₂⋅rᴿ]
  --   p₃ = if CS.null l₃ then [] else [(Lit l₃)⋅r₁ :⊑ r₃] 
  --   la2 = l₂ ∖ l₃
  --   la3 = l₃ ∖ l₂
  --   la23 = l₂ ∩ l₃
  --   pamb = [[(Lit la23)⋅r₁ :⊑ r₂⋅rᴿ] ++ pa2 ++ pa3, [(Lit la23)⋅r₁ :⊑ r₃] ++ pa2 ++ pa3]
  --   pa2 = if CS.null la2 then [] else [(Lit la2)⋅r₁ :⊑ r₂⋅rᴿ]
  --   pa3 = if CS.null la2 then [] else [(Lit la3)⋅r₁ :⊑ r₃]

  -- LetterChoice + ElimCat --------------------------------------
  Lit l :⋅ r₁ :⊑ r₅@(r₂ :+ r₃) :⋅ r₄
    | letterChoice_2, letterChoice_3, elimCat -> trace "LetterChoice_1 ***" [[rᴸ :⊑ r₂⋅r₄],[rᴸ :⊑ r₃⋅r₄],[rᴸ :⊑ r₄]]
    | letterChoice_2, letterChoice_3          -> trace "LetterChoice_2 **" [[rᴸ :⊑ r₂⋅r₄],[rᴸ :⊑ r₃⋅r₄]]
    | letterChoice_2, elimCat                 -> trace "LetterChoice_3 **" [[rᴸ :⊑ r₂⋅r₄],[rᴸ :⊑ r₄]]
    | letterChoice_3, elimCat                 -> trace "LetterChoice_4 **" [[rᴸ :⊑ r₃⋅r₄],[rᴸ :⊑ r₄]]
    | letterChoice_2                          -> trace "LetterChoice_5" [[rᴸ :⊑ r₂⋅r₄]]
    | letterChoice_3                          -> trace "LetterChoice_6" [[rᴸ :⊑ r₃⋅r₄]]
    | elimCat                                 -> trace "LetterChoice_7" [[rᴸ :⊑ r₄]]
   where
    letterChoice_2 = l ⊆ first r₂
    letterChoice_3 = l ⊆ first r₃
    elimCat = 𝔫 r₅ && l ⊆ first r₄

  -- -- LetterChoice + ElimCat + LeftChoice --------------------------------------
  -- Lit l :⋅ r₁ :⊑ r₅@(r₂ :+ r₃) :⋅ r₄
  --   | l₂ ∪ l₃ ∪ l₄ /= l                 -> trace "LetterChoice_1" []                   -- no match    
  --   | l₂ ∩? l₃, l₂ ∩? l₄, l₃ ∩? l₄ -> undefined -- TODO
  --   | l₂ ∩? l₃, l₂ ∩? l₄
  --   | l₂ ∩? l₃, l₃ ∩? l₄
  --   | l₂ ∩? l₄, l₃ ∩? l₄
  --   | l₂ ∩? l₃ -> 
  --   | l₂ ∩? l₄ -> 
  --   | l₃ ∩? l₄
  --   | otherwise                         -> trace "LetterChoice_3" [p₂ ++ p₃ ++ p₄]
  --  where
  --   l₂ = l ⩀ r₂
  --   l₃ = l ⩀ r₃
  --   l₄ = if 𝔫 r₅ then l ⩀ r₄ else mempty
  --   p₂ = if CS.null l₂ then [] else [(Lit l₂)⋅r₁ :⊑ r₂⋅r₄]
  --   p₃ = if CS.null l₃ then [] else [(Lit l₃)⋅r₁ :⊑ r₃⋅r₄]
  --   p₄ = if CS.null l₄ then [] else [(Lit l₄)⋅r₁ :⊑ r₄]
  
  -- LeftChoice ---------------------------------------------------------------
  (r₁ :+ r₂) :⋅ r₃ :⊑ r₄              -> trace "LeftChoice" [[r₁⋅r₃ :⊑ r₄, r₂⋅r₃ :⊑ r₄]]

  -- LeftStar + ElimCat -------------------------------------------------------
  Star r₁ :⋅ r₂ :⊑ r₃ :⋅ r₄
    -- | leftStar, elimCat, first rᴸ ∩ first r₃ /= first rᴸ ∩ first r₄ -> []
    | leftStar, elimCat               -> trace "LeftStar_1 **" [[rᴸ :⊑ r₄], [r₁⋅rᴸ :⊑ rᴿ, r₂ :⊑ rᴿ]]
    | leftStar                        -> trace "LeftStar_2" [[r₁⋅rᴸ :⊑ rᴿ, r₂ :⊑ rᴿ]]
    | elimCat                         -> trace "LeftStar_3" [[rᴸ :⊑ r₄]]
   where
    leftStar = (isLit r₃ || isStar r₃) && rᴸ ⊙ r₃
    elimCat  = 𝔫 r₃ && rᴸ ⋖ r₄

  -- StarChoice1 + StarChoice2 + ElimCat --------------------------------------
  Star r₁ :⋅ r₂ :⊑ r₆@(r₃ :+ r₄) :⋅ r₅
    | amb -> trace "StarChoice_1 ****" mkBranch [p1,p2,p3,p4]
    | sc13                   -> trace "StarChoice_2"  [p2]
    | sc14                   -> trace "StarChoice_3"  [p3]
    | sc2                     -> trace "StarChoice_4"  [p4]
    | ec                      -> trace "StarChoice_5" [p1]
   where
    ec       = 𝔫 r₆ && rᴸ ⋖ r₅
    sc13 = rᴸ ⊙ r₃ && rᴸ ⋖ r₃⋅r₅ && (𝔫̸ r₂ || 𝔫 r₃)
    sc14 = rᴸ ⊙ r₄ && rᴸ ⋖ r₄⋅r₅ && (𝔫̸ r₂ || 𝔫 r₄)
    sc2   = rᴸ ⊙ r₆ &&
                    ((𝔫̸ r₄ && (𝔫 r₂ || rᴸ ⊙ r₃⋅r₅)) || rᴸ ⊙ r₃) &&
                    ((𝔫̸ r₃ && (𝔫 r₂ || rᴸ ⊙ r₄⋅r₅)) || rᴸ ⊙ r₄)
    amb = sum (map fromEnum [sc13, sc14, sc2, ec]) > 1 
    p1 = if ec then [rᴸ :⊑ r₅] else []
    p2 = if sc13 then [rᴸ :⊑ r₃⋅r₅] else []
    p3 = if sc14 then [rᴸ :⊑ r₄⋅r₅] else []
    p4 = if sc2 then [r₁⋅rᴸ :⊑ rᴿ, r₂ :⊑ rᴿ] else []

  -----------------------------------------------------------------------------
  _ -> trace "nothing"  assert (not (rᴸ ⋖ rᴿ) || (𝔫 rᴸ && 𝔫̸ rᴿ) || (rᴸ /= One && rᴿ == One)) []


mkBranch :: [[a]] -> [[a]]
mkBranch = filter (not . null)

