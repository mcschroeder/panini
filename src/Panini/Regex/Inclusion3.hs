{-# OPTIONS_GHC -Wno-operator-whitespace #-}
{-# LANGUAGE TypeFamilies #-}

{-
This module implements fast regular expression inclusion testing for certain
kinds of expressions.

References:

  * Hovland, Dag. 2012. "The inclusion problem for regular expressions." 
    Journal of Computer and System Sciences 78 (2012): 1795-1813.
    https://doi.org/10.1016/j.jcss.2011.12.003

-}
module Panini.Regex.Inclusion3 (isIncludedBy3) where

import Algebra.Lattice
import Control.Exception
import Data.Foldable
import Data.Maybe
import Data.Set qualified as Set
import Data.Set (Set)
import Panini.Regex.CharSet (CharSet)
import Panini.Regex.CharSet qualified as CS
import Panini.Regex.Type
import Prelude
import Panini.Regex.Derivative
import Debug.Trace
import Panini.Pretty
import Control.Monad.ST
import Data.STRef
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)


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

-------------------------------------------------------------------------------

-- -- | The result of the regular-language inclusion test @a `isIncludedBy` b@.
-- data Result
--   = Yes           -- ^ L(a) ⊆ L(b)
--   | No            -- ^ L(a) ⊈ L(b)
--   | OneAmbiguous  -- ^ b is 1-ambiguous in a problematic way
--   deriving stock (Eq, Ord, Show, Read)

-- | A polynomial-time regular language inclusion test, based on the algorithm
-- by Dag Hovland (2012). Both the left-hand and the right-hand expression can
-- be arbitrary. If the right-hand expression is 1-unambiguous, then the
-- algorithm is guaranteed to decide the inclusion problem. However, if the
-- right-hand expression is 1-ambiguous, then the algorithm might either decide
-- the problem correctly, or complain about the 1-ambiguity.
isIncludedBy3 :: Regex -> Regex -> Bool
isIncludedBy3 r₁ r₂ = runST $ do
  dcache <- newSTRef mempty

  let deriv c r = {-# SCC "isIncludedBy3_deriv" #-} do
        Map.lookup (r,c) <$> readSTRef dcache >>= \case
          Just d -> return d
          Nothing -> do
            let d = derivative c r
            modifySTRef' dcache $ Map.insert (r,c) d
            return d
  
  let derivs r1 r2 c = {-# SCC "isIncludedBy3_derivs" #-} do
        d1 <- deriv c r1
        d2 <- deriv c r2
        return (d1 :⊑ d2)
  
  let go _ _ [] = {-# SCC "isIncludedBy3_go_1" #-} return True
      go !(c :: Int) s (i@(r1 :⊑ r2):t)
        | c > 20 = return False        
        | i ∈ s     = {-# SCC "isIncludedBy3_go_2" #-} go c s t
        | otherwise = case match i of
            NoMatch     -> {-# SCC "isIncludedBy3_go_3" #-} return False
            Premises ps -> {-# SCC "isIncludedBy3_go_4" #-} go (c + 1) (Set.insert i s) (ps ++ t)
            Ambiguous   -> {-# SCC "isIncludedBy3_go_5" #-} do
              ps <- mapM (derivs r1 r2) [c | a <- Set.toList $ next r1 ⋉ next r2, Just c <- [CS.choose a] ]
              go (c + 1) (Set.insert i s) (ps ++ t)
  
  go 0 mempty [r₁ :⊑ r₂]

  
                    



-- Note: Our 'Regex' type already insures that expressions are in header-form,
-- hence the omission of the `hdf` function (Hovland 2012, Definition 2.7).
--
-- Note also that we omit the pre-emptive no-match check performed by Hovland's
-- algorithm (2012, Fig. 2), since our 'match' function does this implicitly.

data MatchResult = Premises [Regex ⊑ Regex] | Ambiguous | NoMatch

-- | The 'match' function models an inference system for the regular expression
-- inclusion relation '⊑' (Hovland 2012, Table 2). Given a conclusion, it
-- returns either the premises necessary to infer it or reports some kind of
-- failure. In particular, @'match' (r₁ ':⊑' r₂)@ returns either
--
--  (1) a list of 'Premises', i.e., inclusion relations between subexpressions
--      of @r₁@ and @r₂@ that also need to be true to support @r₁ '⊑' r₂@;
--
--  (2) a report of 'Ambiguous' inference, in which case the 1-ambiguity of @r₂@
--      has caused multiple rules to match and the conclusion may or may not be
--      supportable;
--
--  (3) or 'NoMatch', which means @r₁ ⋢ r₂@.
--
--
-- === __Implementation Notes__
-- 
-- * For performance reasons, our implementation combines ambiguous rules to
--   match them "all at once". This finds ambiguities immediately and avoids
--   having to exhaustively match all rules one after another. 
-- 
-- * Because in our regex representation literals @l@ are really character sets
--   @{l₁,…,lₙ}@, they must be treated as choices @(l₁ + ⋯ + lₙ)@. Specifically,
--   our versions of the LetterStar and LetterChoice rules integrate the
--   LeftChoice rule by splitting the character set depending on which
--   characters match which (sub-)rule, taking care to detect ambiguous matches.
match :: (Regex ⊑ Regex) -> MatchResult
match conclusion@(rᴸ :⊑ rᴿ) = case conclusion of
  -- Axm ----------------------------------------------------------------------
  One :⊑ r 
    | 𝔫 r                             -> {-# SCC "match_Axm" #-} Premises []

  -- Letter -------------------------------------------------------------------
  Lit l₁ :⋅ r₁ :⊑ Lit l₂  :⋅ r₂ 
    | l₁ ⊆ l₂                         -> {-# SCC "match_Letter" #-} Premises [r₁ :⊑ r₂]

  -- LetterStar + ElimCat + LeftChoice ----------------------------------------
  Lit l :⋅ r₁ :⊑ Star r₂ :⋅ r₃
    | l₂ ∩? l₃                        -> {-# SCC "match_LetterStar_1" #-} Ambiguous
    | l₂ ∪ l₃ == l                    -> {-# SCC "match_LetterStar_2" #-} Premises (p₂ ++ p₃)
   where
    l₂ = l ⩀ r₂
    l₃ = l ⩀ r₃
    p₂ = if CS.null l₂ then [] else [(Lit l₂)⋅r₁ :⊑ r₂⋅rᴿ]
    p₃ = if CS.null l₃ then [] else [(Lit l₃)⋅r₁ :⊑ r₃]

  -- LetterChoice + ElimCat + LeftChoice --------------------------------------
  Lit l :⋅ r₁ :⊑ r₅@(r₂ :+ r₃) :⋅ r₄
    | l₂ ∩? l₃                        -> {-# SCC "match_LetterChoice_1" #-} Ambiguous
    | l₂ ∩? l₄                        -> {-# SCC "match_LetterChoice_2" #-} Ambiguous
    | l₃ ∩? l₄                        -> {-# SCC "match_LetterChoice_3" #-} Ambiguous
    | l₂ ∪ l₃ ∪ l₄ == l               -> {-# SCC "match_LetterChoice_4" #-} Premises (p₂ ++ p₃ ++ p₄)
   where
    l₂ = l ⩀ r₂
    l₃ = l ⩀ r₃
    l₄ = if 𝔫 r₅ then l ⩀ r₄ else mempty
    p₂ = if CS.null l₂ then [] else [(Lit l₂)⋅r₁ :⊑ r₂⋅r₄]
    p₃ = if CS.null l₃ then [] else [(Lit l₃)⋅r₁ :⊑ r₃⋅r₄]
    p₄ = if CS.null l₄ then [] else [(Lit l₄)⋅r₁ :⊑ r₄]
  
  -- LeftChoice ---------------------------------------------------------------
  (r₁ :+ r₂) :⋅ r₃ :⊑ r₄              -> {-# SCC "match_LeftChoice" #-} Premises [r₁⋅r₃ :⊑ r₄, r₂⋅r₃ :⊑ r₄]

  -- LeftStar + ElimCat -------------------------------------------------------
  Star r₁ :⋅ r₂ :⊑ r₃ :⋅ r₄
    | leftStar, elimCat               -> {-# SCC "match_LeftStar_1" #-} Ambiguous
    | leftStar                        -> {-# SCC "match_LeftStar_2" #-} Premises [r₁⋅rᴸ :⊑ rᴿ, r₂ :⊑ rᴿ]
    | elimCat                         -> {-# SCC "match_LeftStar_3" #-} Premises [rᴸ :⊑ r₄]
   where
    leftStar = (isLit r₃ || isStar r₃) && rᴸ ⊙ r₃
    elimCat  = 𝔫 r₃ && rᴸ ⋖ r₄

  -- StarChoice1 + StarChoice2 + ElimCat --------------------------------------
  Star r₁ :⋅ r₂ :⊑ r₆@(r₃ :+ r₄) :⋅ r₅
    | starChoice1_3, starChoice1_4    -> {-# SCC "match_StarChoice_1" #-} Ambiguous
    | starChoice1_3, starChoice2      -> {-# SCC "match_StarChoice_2" #-} Ambiguous
    | starChoice1_4, starChoice2      -> {-# SCC "match_StarChoice_3" #-} Ambiguous
    | starChoice1_3, elimCat          -> {-# SCC "match_StarChoice_4" #-} Ambiguous
    | starChoice1_4, elimCat          -> {-# SCC "match_StarChoice_5" #-} Ambiguous
    | starChoice2, elimCat            -> {-# SCC "match_StarChoice_6" #-} Ambiguous
    | starChoice1_3                   -> {-# SCC "match_StarChoice_7" #-} Premises [rᴸ :⊑ r₃⋅r₅] 
    | starChoice1_4                   -> {-# SCC "match_StarChoice_8" #-} Premises [rᴸ :⊑ r₄⋅r₅] 
    | starChoice2                     -> {-# SCC "match_StarChoice_9" #-} Premises [r₁⋅rᴸ :⊑ rᴿ, r₂ :⊑ rᴿ]
    | elimCat                         -> {-# SCC "match_StarChoice_10" #-} Premises [rᴸ :⊑ r₅]
   where
    elimCat       = 𝔫 r₆ && rᴸ ⋖ r₅
    starChoice1_3 = rᴸ ⊙ r₃ && rᴸ ⋖ r₃⋅r₅ && (𝔫̸ r₂ || 𝔫 r₃)
    starChoice1_4 = rᴸ ⊙ r₄ && rᴸ ⋖ r₄⋅r₅ && (𝔫̸ r₂ || 𝔫 r₄)
    starChoice2   = rᴸ ⊙ r₆ &&
                    ((𝔫̸ r₄ && (𝔫 r₂ || rᴸ ⊙ r₃⋅r₅)) || rᴸ ⊙ r₃) &&
                    ((𝔫̸ r₃ && (𝔫 r₂ || rᴸ ⊙ r₄⋅r₅)) || rᴸ ⊙ r₄)

  -----------------------------------------------------------------------------
  _ -> {-# SCC "match_Nothing" #-} assert (not (rᴸ ⋖ rᴿ) || (𝔫 rᴸ && 𝔫̸ rᴿ) || (rᴸ /= One && rᴿ == One)) NoMatch


ambiguous i@(r :⊑ s) = [derivative c r :⊑ derivative c s | a <- Set.toList $ next' i, Just c <- [CS.choose a]]

next' :: (Regex ⊑ Regex) -> Set CharSet
next' (r :⊑ s) = next r ⋉ next s

{-# SCC isIncludedBy3 #-}
{-# SCC match #-}
{-# SCC ambiguous #-}
{-# SCC next' #-}
