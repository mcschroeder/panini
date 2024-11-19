{-
This module implements fast regular expression inclusion testing for certain
kinds of expressions.

References:

  * Hovland, Dag. 2010. "The Inclusion Problem for Regular Expressions." 
    LATA 2010. LNCS 6031: 309–320. https://doi.org/10.1007/978-3-642-13089-2_26

  * Hovland, Dag. 2012. "The inclusion problem for regular expressions." 
    Journal of Computer and System Sciences 78 (2012): 1795-1813.
    https://doi.org/10.1016/j.jcss.2011.12.003

-}
module Panini.Regex.Inclusion (isIncludedBy, Result(..)) where

import Data.Foldable
import Data.Maybe
import Data.Set qualified as Set
import Panini.Regex.CharSet (CharSet)
import Panini.Regex.CharSet qualified as CS
import Panini.Regex.Type
import Prelude

-------------------------------------------------------------------------------

-- We will use some notational conveniences throughout this module:

(∈) :: Eq a => Foldable t => a -> t a -> Bool
(∈) = elem

(⊆) :: CharSet -> CharSet -> Bool
(⊆) = CS.isSubsetOf

(⊈) :: CharSet -> CharSet -> Bool
a ⊈ b = not (CS.isSubsetOf a b)

(><) :: CharSet -> CharSet -> Bool
a >< b = not $ CS.null $ CS.intersection a b

-- | The '⊑' type is a reified regular expression inclusion relation.
data a ⊑ b = a :⊑ b
  deriving stock (Eq, Ord, Show, Read)

-------------------------------------------------------------------------------

-- | The result of the regular-language inclusion test @a `isIncludedBy` b@.
data Result
  = Yes           -- ^ L(a) ⊆ L(b)
  | No            -- ^ L(a) ⊈ L(b)
  | OneAmbiguous  -- ^ b is 1-ambiguous in a problematic way
  deriving stock (Eq, Ord, Show, Read)

-- | A polynomial-time regular language inclusion test, based on the algorithm
-- by Dag Hovland (2010). Both the left-hand and the right-hand expression can
-- be arbitrary. If the right-hand expression is 1-unambiguous, then the
-- algorithm is guaranteed to decide the inclusion problem. However, if the
-- right-hand expression is 1-ambiguous, then the algorithm might either decide
-- the problem correctly, or complain about the 1-ambiguity.
isIncludedBy :: Regex -> Regex -> Result
isIncludedBy a b = go [a :⊑ b] mempty
 where
  go [] _                            = Yes
  go (i@(r1 :⊑ r2):t) s
    | i ∈ s                          = go t s
    | first r1 ⊈ first r2            = No
    | nullable r1, not (nullable r2) = No
    | r2 == One, r1 /= One           = No
    | otherwise                      = case premises i of
                                        []   -> No
                                        [ps] -> go (ps ++ t) (Set.insert i s)
                                        _    -> OneAmbiguous

-- Note: Our 'Regex' type already insures that expressions are in header-form,
-- hence the omission of the `hdf` function (Hovland 2010, Definition 7).

isLit :: Regex -> Bool
isLit = \case
  Lit _ -> True
  _     -> False

isStar :: Regex -> Bool
isStar = \case
  Star _ -> True
  _      -> False

-- | The set of letters that can occur first in a word in the language, i.e.,
-- first(r) = {l ∈ Σ ∣ ∃w: l⋅w ∈ L(r)} (Hovland 2010, Definition 3).
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

-- TODO: note: this is actually the system from an earlier publication (Springer)
-- the 2012 paper has slightly different rules. is it worth it to update?

-- | This function models an inference system for the regular expression
-- inclusion relation '⊑' (Hovland 2010, Table 1). 
--
-- @'premises' (a ':⊑' b)@ returns a list of premises, i.e., a list of inclusion
-- relations between subexpressions of @a@ and @b@ that need to be true to
-- support the conclusion @a '⊑' b@. Ideally, the return value is a singleton
-- list of premises, corresponding to a single matching rule instance. However,
-- if the result is a list containing more than one list of premises, then the
-- 1-ambiguity of @b@ has caused multiple rules to match and the conclusion is
-- not necessarily sound. Similarly, the conclusion cannot be supported if no
-- rule matches, in which case the return value is an entirely empty list. Note
-- that the latter case is different from the return value being a singleton
-- list containing an empty list, i.e., an axiomatic premise.
--
-- Due to differences in representing regular expressions, our inference system
-- is slightly different to Hovland's, most notably in explicitly distinguishing
-- between expressions of the form r1⋅r2 (where r2 ≠ ε) and r1⋅ε (which is
-- simply r1 in our system). We have also added the additional rules LeftOpt,
-- LetterOpt, StarOpt1, and StarOpt2 since we explicitly encode optional forms.
--
-- Note also that the inference system presented in the 2012 JCSS paper is
-- slightly different from the one in the earlier 2010 LATA paper. Our
-- implementation is based on the 2010 version.
premises :: (Regex ⊑ Regex) -> [[Regex ⊑ Regex]]
premises conclusion = catMaybes $ map ($ conclusion) $
  [ \case ---------------------------------------------------------------------
  One :⊑ r                                                               -- Axm
    | nullable r 
                                  -> Just []
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  (Times1 (Lit l1) r1) :⊑ (Times1 (Lit l2) r2)                        -- Letter
    | l1 ⊆ l2 
                                  -> Just [r1 :⊑ r2]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  (Times1 (Lit l1) r1) :⊑ Lit l2                                      -- Letter
    | l1 ⊆ l2
                                  -> Just [r1 :⊑ One]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  Lit l1 :⊑ (Times1 (Lit l2) r2)                                      -- Letter
    | l1 ⊆ l2
                                  -> Just [One :⊑ r2]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  Lit l1 :⊑ Lit l2                                                    -- Letter
    | l1 ⊆ l2                         
                                  -> Just []
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Times1 (Lit l) _) :⊑ rhs@(Times1 (Star r2) _)              -- LetterStar
    | l ⊆ first r2  
                                  -> Just [lhs :⊑ (r2 <> rhs)]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Times1 (Lit l) _) :⊑ rhs@(Star r2)                         -- LetterStar
    | l ⊆ first r2 
                                  -> Just [lhs :⊑ (r2 <> rhs)]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Lit l) :⊑ rhs@(Times1 (Star r2) _)                         -- LetterStar
    | l ⊆ first r2 
                                  -> Just [lhs :⊑ (r2 <> rhs)]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Lit l) :⊑ rhs@(Star r2)                                    -- LetterStar
    | l ⊆ first r2 
                                  -> Just [lhs :⊑ (r2 <> rhs)]
  _                               -> Nothing  
  , \case ---------------------------------------------------------------------
  lhs@(Times1 (Lit l) _) :⊑ Times1 (Plus1 r2 _) r4              -- LetterChoice
    | l ⊆ first r2                    
                                  -> Just [lhs :⊑ (r2 <> r4)]
  _                               -> Nothing  
  , \case ---------------------------------------------------------------------
  lhs@(Times1 (Lit l) _) :⊑ Times1 (Plus1 _ r3) r4              -- LetterChoice
    | l ⊆ first r3
                                  -> Just [lhs :⊑ (r3 <> r4)]
  _                               -> Nothing  
  , \case ---------------------------------------------------------------------
  lhs@(Times1 (Lit l) _) :⊑ Plus1 r2 _                          -- LetterChoice
    | l ⊆ first r2 
                                  -> Just [lhs :⊑ r2]
  _                               -> Nothing  
  , \case ---------------------------------------------------------------------
  lhs@(Times1 (Lit l) _) :⊑ Plus1 _ r3                          -- LetterChoice
    | l ⊆ first r3
                                  -> Just [lhs :⊑ r3]
  _                               -> Nothing  
  , \case ---------------------------------------------------------------------
  lhs@(Lit l) :⊑ Times1 (Plus1 r2 _) r4                         -- LetterChoice
    | l ⊆ first r2 
                                  -> Just [lhs :⊑ (r2 <> r4)]
  _                               -> Nothing  
  , \case ---------------------------------------------------------------------
  lhs@(Lit l) :⊑ Times1 (Plus1 _ r3) r4                         -- LetterChoice
    | l ⊆ first r3 
                                  -> Just [lhs :⊑ (r3 <> r4)]
  _                               -> Nothing  
  , \case ---------------------------------------------------------------------
  lhs@(Lit l) :⊑ Plus1 r2 _                                     -- LetterChoice
    | l ⊆ first r2 
                                  -> Just [lhs :⊑ r2]
  _                               -> Nothing  
  , \case ---------------------------------------------------------------------
  lhs@(Lit l) :⊑ Plus1 _ r3                                     -- LetterChoice
    | l ⊆ first r3 
                                  -> Just [lhs :⊑ r3]
  _                               -> Nothing    
  , \case ---------------------------------------------------------------------
  lhs@(Times1 (Lit l) _) :⊑ Times1 (Opt r2) r4                     -- LetterOpt
    | l ⊆ first r2
                                  -> Just [lhs :⊑ (r2 <> r4)]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Times1 (Lit l) _) :⊑ Opt r2                                 -- LetterOpt
    | l ⊆ first r2 
                                  -> Just [lhs :⊑ r2]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Lit l) :⊑ Times1 (Opt r2) r4                                -- LetterOpt
    | l ⊆ first r2 
                                  -> Just [lhs :⊑ (r2 <> r4)]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Lit l) :⊑ Opt r2                                            -- LetterOpt
    | l ⊆ first r2 
                                  -> Just [lhs :⊑ r2]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  Star r1 :⊑ Star r2                                               -- StarStarE
                                  -> Just [r1 :⊑ Star r2]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Times1 (Star _) r2) :⊑ Times1 (Plus1 r3 _) r5             -- StarChoice1
    | let first_lhs = first lhs
    , let r35 = r3 <> r5
    , first_lhs >< first r3
    , first_lhs ⊆ first r35
    , not (nullable r2) || nullable r3
                                  -> Just [lhs :⊑ r35]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Times1 (Star _) r2) :⊑ Times1 (Plus1 _ r4) r5             -- StarChoice1
    | let first_lhs = first lhs
    , let r45 = r4 <> r5
    , first_lhs >< first r4
    , first_lhs ⊆ first r45
    , not (nullable r2) || nullable r4
                                  -> Just [lhs :⊑ r45]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Times1 (Star _) r2) :⊑ Plus1 r3 _                         -- StarChoice1
    | let first_lhs = first lhs
    , first_lhs >< first r3
    , first_lhs ⊆ first r3
    , not (nullable r2) || nullable r3
                                  -> Just [lhs :⊑ r3]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Times1 (Star _) r2) :⊑ Plus1 _ r4                         -- StarChoice1
    | let first_lhs = first lhs
    , first_lhs >< first r4
    , first_lhs ⊆ first r4
    , not (nullable r2) || nullable r4
                                  -> Just [lhs :⊑ r4]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Star _) :⊑ Times1 (Plus1 r3 _) r5                         -- StarChoice1
    | let first_lhs = first lhs
    , let r35 = r3 <> r5
    , first_lhs >< first r3
    , first_lhs ⊆ first r35
    , nullable r3
                                  -> Just [lhs :⊑ r35]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Star _) :⊑ Times1 (Plus1 _ r4) r5                         -- StarChoice1
    | let first_lhs = first lhs
    , let r45 = r4 <> r5
    , first_lhs >< first r4
    , first_lhs ⊆ first r45
    , nullable r4
                                  -> Just [lhs :⊑ r45]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Star _) :⊑ Plus1 r3 _                                     -- StarChoice1
    | first lhs ⊆ first r3
    , nullable r3
                                  -> Just [lhs :⊑ r3]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Star _) :⊑ Plus1 _ r4                                     -- StarChoice1
    | first lhs ⊆ first r4
    , nullable r4
                                  -> Just [lhs :⊑ r4]  
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Times1 (Star r1) r2) :⊑ rhs@(Times1 r34@(Plus1 r3 r4) r5) -- StarChoice2
    | let first_lhs = first lhs
    , first_lhs >< first r34
    , (nullable r2 && not (nullable r3)) || first_lhs ⊈ first (r3 <> r5)
    , (nullable r2 && not (nullable r4)) || first_lhs ⊈ first (r4 <> r5)
                                  -> Just [(r1 <> lhs) :⊑ rhs, r2 :⊑ rhs]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Times1 (Star r1) r2) :⊑ r34@(Plus1 r3 r4)                 -- StarChoice2
    | let first_lhs = first lhs
    , first_lhs >< first r34
    , (nullable r2 && not (nullable r3)) || first_lhs ⊈ first r3
    , (nullable r2 && not (nullable r4)) || first_lhs ⊈ first r4
                                  -> Just [(r1 <> lhs) :⊑ r34, r2 :⊑ r34]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Star r1) :⊑ rhs@(Times1 r34@(Plus1 r3 r4) r5)             -- StarChoice2
    | let first_lhs = first lhs
    , first_lhs >< first r34
    , not (nullable r3) || first_lhs ⊈ first (r3 <> r5)
    , not (nullable r4) || first_lhs ⊈ first (r4 <> r5)
                                  -> Just [(r1 <> lhs) :⊑ rhs, One :⊑ rhs]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Star r1) :⊑ r34@(Plus1 r3 r4)                             -- StarChoice2
    | let first_lhs = first lhs
    , first_lhs >< first r34
    , not (nullable r3) || first_lhs ⊈ first r3
    , not (nullable r4) || first_lhs ⊈ first r4
                                  -> Just [(r1 <> lhs) :⊑ r34, One :⊑ r34]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Times1 (Star _) r2) :⊑ Times1 (Opt r3) r5                    -- StarOpt1
    | let first_lhs = first lhs
    , let r35 = r3 <> r5
    , first_lhs >< first r3
    , first_lhs ⊆ first r35
    , not (nullable r2) || nullable r3
                                  -> Just [lhs :⊑ r35]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Times1 (Star _) r2) :⊑ Opt r3                                -- StarOpt1
    | let first_lhs = first lhs
    , first_lhs >< first r3
    , first_lhs ⊆ first r3
    , (not (nullable r2) || nullable r3)
                                  -> Just [lhs :⊑ r3]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Star _) :⊑ Times1 (Opt r3) r5                                -- StarOpt1
    | let first_lhs = first lhs
    , let r35 = r3 <> r5
    , first_lhs >< first r3
    , first_lhs ⊆ first r35
    , nullable r3
                                  -> Just [lhs :⊑ r35]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Star _) :⊑ Opt r3                                            -- StarOpt1
    | first lhs ⊆ first r3
    , nullable r3
                                  -> Just [lhs :⊑ r3]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------  
  lhs@(Times1 (Star r1) r2) :⊑ rhs@(Times1 (Opt r3) r5)             -- StarOpt2
    | let first_lhs = first lhs
    , first_lhs >< first r3
    , ((nullable r2 && not (nullable r3)) || first_lhs ⊈ first (r3 <> r5))
    , first_lhs ⊈ first r5
                                  -> Just [(r1 <> lhs) :⊑ rhs, r2 :⊑ rhs]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Times1 (Star r1) r2) :⊑ rhs@(Opt r3)                         -- StarOpt2
    | let first_lhs = first lhs
    , first_lhs >< first rhs
    , ((nullable r2 && not (nullable r3)) || first_lhs ⊈ first r3)
                                  -> Just [(r1 <> lhs) :⊑ rhs, r2 :⊑ rhs]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Star r1) :⊑ rhs@(Times1 (Opt r3) r5)                         -- StarOpt2
    | let first_lhs = first lhs
    , first_lhs >< first r3
    , (not (nullable r3) || first_lhs ⊈ first (r3 <> r5))
    , first_lhs ⊈ first r5
                                  -> Just [(r1 <> lhs) :⊑ rhs, One :⊑ rhs]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Star r1) :⊑ rhs@(Opt r3)                                     -- StarOpt2
    | let first_lhs = first lhs
    , first_lhs >< first rhs
    , (not (nullable r3) || first_lhs ⊈ first r3)
                                  -> Just [(r1 <> lhs) :⊑ rhs, One :⊑ rhs]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Times1 (Star r1) r2) :⊑ rhs@(Times1 r3 _)                    -- LeftStar
    | first r1 >< first r3
    , isLit r3 || isStar r3
                                  -> Just [(r1 <> lhs) :⊑ rhs, r2 :⊑ rhs]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Times1 (Star r1) r2) :⊑ r3                                   -- LeftStar
    | first r1 >< first r3
    , isLit r3 || isStar r3
                                  -> Just [(r1 <> lhs) :⊑ r3, r2 :⊑ r3]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  lhs@(Star r1) :⊑ rhs@(Times1 r3 _)                                -- LeftStar
    | first r1 >< first r3
    , isLit r3 || isStar r3
                                  -> Just [(r1 <> lhs) :⊑ rhs, One :⊑ rhs]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  Times1 (Plus1 r1 r2) r3 :⊑ r4                                   -- LeftChoice
                                  -> Just [(r1 <> r3) :⊑ r4, (r2 <> r3) :⊑ r4]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  Plus1 r1 r2 :⊑ r4                                               -- LeftChoice
                                  -> Just [r1 :⊑ r4, r2 :⊑ r4]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  Times1 (Opt r1) r3 :⊑ r4                                           -- LeftOpt
                                  -> Just [(r1 <> r3) :⊑ r4, r3 :⊑ r4]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  Opt r1 :⊑ r4                                                       -- LeftOpt
                                  -> Just [r1 :⊑ r4, One :⊑ r4]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  r1@(Times1 r4 _) :⊑ Times1 r2 r3                                   -- ElimCat
    | isLit r4 || isStar r4
    , nullable r2
    , first r1 ⊆ first r3
                                  -> Just [r1 :⊑ r3]
  _                               -> Nothing
  , \case ---------------------------------------------------------------------
  r1 :⊑ Times1 r2 r3                                                 -- ElimCat
    | isLit r1 || isStar r1
    , nullable r2
    , first r1 ⊆ first r3
                                  -> Just [r1 :⊑ r3]
  _                               -> Nothing  
  ] ---------------------------------------------------------------------------
