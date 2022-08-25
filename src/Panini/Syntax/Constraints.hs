module Panini.Syntax.Constraints where

import Panini.Syntax.Names
import Panini.Syntax.Primitives
import Panini.Syntax.Predicates
import Prelude

------------------------------------------------------------------------------

-- | Constraints are Horn clauses in negation normal form (NNF). They form a
-- tree, where each leaf is a goal ('CHead') and each node either quantifies
-- some variable to satisfy a hypothesis ('CAll') or conjoins two
-- sub-constraints ('CAnd').
data Con
  = CHead Pred               -- p
  | CAnd Con Con             -- c1 /\ c2
  | CAll Name Base Pred Con  -- forall x:b. p ==> c
  deriving stock (Eq, Show, Read)

pattern CTrue :: Con
pattern CTrue = CHead PTrue

-- | Smart constructor for `CAnd`, eliminates redundant true values.
cAnd :: Con -> Con -> Con
cAnd CTrue c2    = c2
cAnd c1    CTrue = c1
cAnd c1    c2    = CAnd c1 c2
