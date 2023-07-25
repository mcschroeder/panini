module Panini.SMT.RegLan where

import Prelude

-- | A type corresponding to the @RegLan@ sort of SMT-LIB's string theory [1].
--
-- The purpose of this type is to provide a serializaton target for other
-- representations of regular expressions. Basically, any regex for which we can
-- construct an inhabitant of 'RegLan' can be passed on to an SMT solver
-- (provided it supports the relevant parts of the string theory).
--
-- [1] Clark Barrett, Pascal Fontaine, and Cesare Tinelli. The Satisfiability
--     Modulo Theories Library (SMT-LIB). Version 2.6, 2020. Theory of Strings.
--     http://smtlib.cs.uiowa.edu/theories-UnicodeStrings.shtml
--
data RegLan
  = ToRe String         -- ^ string injection @str.to_re@
  | None                -- ^ the empty set of strings @re.none@
  | All                 -- ^ set of all strings @re.all@
  | AllChar             -- ^ set of all singleton strings @re.allchar@
  | Conc RegLan RegLan  -- ^ concatenation @re.++@
  | Union RegLan RegLan -- ^ union @re.union@
  | Inter RegLan RegLan -- ^ intersection @re.inter@
  | Star RegLan         -- ^ Kleene Closure @re.*@
  | Comp RegLan         -- ^ complement @re.comp@
  | Diff RegLan RegLan  -- ^ difference @re.diff@
  | Plus RegLan         -- ^ Kleene cross @re.+@
  | Opt RegLan          -- ^ option @re.opt@
  | Range Char Char     -- ^ set of singleton strings within range @re.range@
  deriving stock (Show, Read, Eq, Ord)
