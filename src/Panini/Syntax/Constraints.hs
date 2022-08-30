module Panini.Syntax.Constraints where

import Panini.Pretty.Printer
import Panini.Syntax.Names
import Panini.Syntax.Predicates
import Panini.Syntax.Primitives
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

------------------------------------------------------------------------------

instance Pretty Con where
  pretty = \case
    CHead p -> pretty p
    CAnd c1 c2 -> pretty c1 <+> symAnd <\> pretty c2
    CAll x b p c -> parens $ case c of
      CHead _ ->          forall_ <+> pretty p <+> symImplies <+> pretty c
      _       -> nest 2 $ forall_ <+> pretty p <+> symImplies <\> pretty c
      where
        forall_ = symAll <> pretty x <> symColon <> pretty b <> symDot
