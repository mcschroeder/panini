module Panini.Solver.Constraints where

import Algebra.Lattice
import Data.Generics.Uniplate.Direct
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

pattern CFalse :: Con
pattern CFalse = CHead PFalse

instance MeetSemilattice Con where
  CTrue ∧ c2    = c2
  c1    ∧ CTrue = c1
  c1    ∧ c2    = CAnd c1 c2

instance BoundedMeetSemilattice Con where
  top = CTrue

instance Uniplate Con where
  uniplate = \case
    CHead p      -> plate CHead |- p
    CAnd c1 c2   -> plate CAnd |* c1 |* c2
    CAll x b p c -> plate CAll |- x |- b |- p |* c

instance Biplate Con Pred where
  biplate = \case
    CHead p      -> plate CHead |* p
    CAnd c1 c2   -> plate CAnd |+ c1 |+ c2
    CAll x b p c -> plate CAll |- x |- b |* p |+ c

instance Pretty Con where
  pretty = \case
    CHead p -> pretty p
    CAnd c1 c2 -> align $ pretty c1 <+> symAnd <\> pretty c2
    CAll x b p c -> parens $ case c of
      CHead _ ->          forall_ <+> pretty p <+> symImplies <+> pretty c
      _       -> hang 2 $ forall_ <+> pretty p <+> symImplies <\> pretty c
      where
        forall_ = symAll <> pretty x <> symColon <> pretty b <> symDot

------------------------------------------------------------------------------

-- | A flat constraint of the form ∀(x₁:b₁,x₂:b₂,…). p ⇒ q where q is either a
-- single κ-variable application κ(y̅) or a concrete predicate free of
-- κ-variables.
data FlatCon = FAll [(Name,Base)] Pred Pred

-- | Flatten a constraint.
flat :: Con -> [FlatCon]
flat c₀ = [simpl [] [PTrue] c' | c' <- split c₀]
  where
    split (CAll x b p c) = [CAll x b p c' | c' <- split c]
    split (CHead p)      = [CHead p]
    split (CAnd c₁ c₂)   = split c₁ ++ split c₂

    simpl xs ps (CAll x b p c) = simpl ((x,b):xs) (p:ps) c
    simpl xs ps (CHead q)      = FAll (reverse xs) (PAnd $ reverse ps) q
    simpl _  _  (CAnd _ _)     = error "impossible"


instance Pretty FlatCon where
  pretty (FAll xs p q) = hang 2 $ sep 
    [ symAll <> binders xs <> symDot
    , pretty p <+> symImplies <+> pretty q
    ]
   where
    binders = prettyTuple . map (\(x,b) -> pretty x <> symColon <> pretty b)
