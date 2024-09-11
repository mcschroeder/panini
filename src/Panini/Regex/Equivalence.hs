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
-}
module Panini.Regex.Equivalence (equivalence, membership) where

import Panini.Regex.CharSet qualified as CS
import Data.Set qualified as Set
import Data.String
import Panini.Regex.Operations
import Panini.Regex.Type
import Prelude

-------------------------------------------------------------------------------

-- | Semantic equivalence of two regular expressions, i.e., language equality.
equivalence :: Regex -> Regex -> Bool
equivalence r1 r2 = equiv [(r1,r2)] []

equiv :: [(Regex,Regex)] -> [(Regex,Regex)] -> Bool
equiv []        _                  = True
equiv ((a,b):s) h | con a /= con b = False
                  | otherwise      = equiv (s ++ s') h'
 where
  a' = det (lin a)
  b' = det (lin b)
  s' = [p | p <- derivatives a' b', p `notElem` h']
  h' = (a,b):h

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
  lin1 (Times (Star a : b))  = Plus [lin1 a <> Star a <> Times b, Times b]
  lin1 (Times (Plus ab : c)) = Plus $ map (lin1 . (<> Times c)) ab
  lin1 (Times ab)            = Times ab
  lin1 (Opt a)               = lin1 a

  lin2 (Plus a)              = Plus $ map lin2 a
  lin2 (Times (Plus ab : c)) = Plus $ map (lin2 . (<> Times c)) ab
  lin2 a                     = a

-- | Given a linear regular expression, return a deterministic linear regular
-- expression.
det :: Regex -> Regex
det (Plus (Times (Lit x : a) : Times (Lit y : b) : c)) | x == y 
      = det $ Plus (Lit x <> Plus [Times a, Times b] : c)
det (Plus [Times (Lit x : a), Times (Lit y : b)]) | x == y 
      = Lit x <> Plus [Times a, Times b]  
det (Plus [Times (Lit x : a), Lit y]) | x == y 
      = Lit x <> Opt (Times a)  
det a = a

-------------------------------------------------------------------------------

-- | Is a given string a member of the language described by the regex?
membership :: String -> Regex -> Bool
membership s r = not $ equivalence (intersection (fromString s) r) Zero
