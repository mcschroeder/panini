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
module Panini.Regex.Equivalence (equivalence, membership) where

import Panini.Regex.CharSet qualified as CS
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Panini.Regex.Derivative
import Panini.Regex.Type
import Prelude

-------------------------------------------------------------------------------

-- | Semantic equivalence of two regular expressions, i.e., language equality.
equivalence :: Regex -> Regex -> Bool
equivalence r1 r2 = equiv (Set.singleton (r1,r2)) mempty

equiv :: Set (Regex,Regex) -> Set (Regex,Regex) -> Bool
equiv s _                                | Set.null s     = True
equiv (Set.deleteFindMin -> ((a,b),s)) h | con a /= con b = False
                                         | otherwise      = equiv (s <> s') h'
 where
  a' = det (lin a)
  b' = det (lin b)
  s' = (Set.fromList $ derivatives a' b') `Set.difference` h'
  h' = Set.insert (a,b) h

derivatives :: Regex -> Regex -> [(Regex,Regex)]
derivatives a b = 
  [ (derivative c a, derivative c b) 
  | p <- Set.toList $ next a ⋈ next b
  , Just c <- [CS.choose p]
  ]

-- | Return the constant part of a regular expression.
con :: Regex -> Regex
con a | nullable a = One
      | otherwise  = Zero

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
