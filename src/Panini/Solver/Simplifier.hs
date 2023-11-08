module Panini.Solver.Simplifier where

import Data.Generics.Uniplate.Operations
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude

simplify :: Con -> Con
simplify = rewrite $ \case
  CAnd (CHead PTrue) c2 -> Just c2
  CAnd c1 (CHead PTrue) -> Just c1
  CAll _ _ _ (CHead PTrue) -> Just (CHead PTrue)
  _ -> Nothing