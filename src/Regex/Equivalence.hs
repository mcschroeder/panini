{-|
This module implements regular expression equivalence testing using a refutation
method presented by Almeida, Moreaira, and Reis (2007). It is based on the
rewrite system originally presented by Antimirov and Mosses (1995).

References:

  * Almeida, Marco, Nelma Moreiara, and Rogério Reis. 2007. "Testing the
    equivalence of regular expressions." Universidade do Porto, Technical report
    series: DCC-2007-07. https://www.dcc.fc.up.pt/Pubs/TR07/dcc-2007-07.pdf

  * Antimirov, Valentin and Peter Mosses. 1995. "Rewriting extended regular
    expressions." Computer Science 143 (1995): 51-72.
    https://doi.org/10.1016/0304-3975(95)80024-4
  
  * Nipkow, Tobias and Dmitriy Traytel. 2014. "Unified Decision Procedures for
    Regular Expression Equivalence." ITP 2014. LNCS 8558: 450-466.
    https://doi.org/10.1007/978-3-319-08970-6_29

-}
module Regex.Equivalence (equivalence, membership) where

import Data.Set qualified as Set
import Data.String
import Prelude
import Regex.CharSet qualified as CS
import Regex.Derivative
import Regex.Type

-------------------------------------------------------------------------------

(∈) :: Eq a => Foldable t => a -> t a -> Bool
(∈) = elem

ν :: Regex -> Bool
ν = nullable

-- | The '≡' type is a reified regular expression equivalence relation.
data a ≡ b = a :≡ b 
  deriving stock (Eq, Ord, Show, Read)

infix 0 :≡

-------------------------------------------------------------------------------

-- | Semantic equivalence of two regular expressions, i.e., language equality.
equivalence :: Regex -> Regex -> Bool
equivalence r1 r2 = go mempty [r1 :≡ r2]
 where
  go _ []               = True
  go h (e@(a :≡ b):s)
    | e ∈ h             = go h s
    | ν a /= ν b        = False
    | otherwise         = go (Set.insert e h) (s' ++ s)
        where
          a' = det (lin a)
          b' = det (lin b)
          s' = [ derivative c a' :≡ derivative c b'
               | p <- Set.toList $ next a ⋈ next b
               , Just c <- [CS.choose p]
               ]

-- | Given an arbitrary regular expression, return a linear regular expression.
lin :: Regex -> Regex
lin = lin2 . lin1
 where
  lin1 Zero                  = Zero
  lin1 One                   = Zero
  lin1 (Lit a)               = Lit a
  lin1 (Plus ab)             = Plus $ map lin1 ab
  lin1 (Star a)              = lin1 a <> Star a
  lin1 (Times1 (Star a) b)   = (lin1 a <> Star a <> b) `plus` b
  lin1 (Times1 (Plus ab) c)  = Plus $ map (lin1 . (<> c)) ab
  lin1 r@(Times _)           = r
  lin1 (Opt a)               = lin1 a

  lin2 (Plus a)              = Plus $ map lin2 a
  lin2 (Times1 (Plus ab) c)  = Plus $ map (lin2 . (<> c)) ab
  lin2 a                     = a

-- | Given a linear regex, return a deterministic linear regex.
det :: Regex -> Regex
det (Plus1 (Times1 (Lit x) a) (Plus1 (Times1 (Lit y) b) c)) 
  | x == y = det $ (Lit x <> (a `plus` b)) `plus` c
det (Plus [Times1 (Lit x) a, Times1 (Lit y) b])
  | x == y = Lit x <> Plus [a,b]  
det (Plus [Times1 (Lit x) a, Lit y]) 
  | x == y = Lit x <> Opt a
det a = a

-------------------------------------------------------------------------------

-- | Is a given string a member of the language described by the regex?
-- Implementation based on `match` function from Nipkow and Traytel (2014).
membership :: String -> Regex -> Bool
membership s r = nullable (foldr derivative r s)
