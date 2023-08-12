module Panini.Syntax.Predicates where

import Algebra.Lattice
import Data.Generics.Uniplate.Direct
import Data.Hashable
import GHC.Generics (Generic)
import Panini.Pretty
import Panini.Syntax.Expressions
import Panini.Syntax.KVar
import Panini.Syntax.Names
import Panini.Syntax.Primitives
import Panini.Syntax.Relations
import Prelude

------------------------------------------------------------------------------

-- | Predicates are terms in a Boolean logic.
data Pred
  = PTrue
  | PFalse
  | PAnd [Pred]             -- ^ conjunction @p₁ ∧ p₂ ∧ … ∧ pₙ@
  | POr [Pred]              -- ^ disjunction @p₁ ∨ p₂ ∨ … ∨ pₙ@
  | PImpl Pred Pred         -- ^ implication @p₁ ⟹ p₂@
  | PIff Pred Pred          -- ^ if-and-only-if @p₁ ⟺ p₂@
  | PNot Pred               -- ^ negation @¬p@
  | PRel Rel                -- ^ relation @e₁ ⋈ e₂@
  | PAppK KVar [Value]      -- ^ κ-variable application @κᵢ(y₁,y₂,…,yₙ)@  
  | PExists Name Base Pred  -- ^ existential quantification @∃x:b. p@
  deriving stock (Eq, Show, Read, Generic)

instance Hashable Pred

instance MeetSemilattice Pred where
  PTrue   ∧ q       = q
  PFalse  ∧ _       = PFalse
  p       ∧ PTrue   = p
  _       ∧ PFalse  = PFalse
  PAnd ps ∧ PAnd qs = PAnd (ps ++ qs)
  PAnd ps ∧ q       = PAnd (ps ++ [q])
  p       ∧ PAnd qs = PAnd (p:qs)
  p       ∧ q       = PAnd [p,q]

instance BoundedMeetSemilattice Pred where
  top = PTrue

instance JoinSemilattice Pred where
  PFalse ∨ q      = q
  PTrue  ∨ _      = PTrue
  p      ∨ PFalse = p
  _      ∨ PTrue  = PTrue
  POr ps ∨ POr qs = POr (ps ++ qs)
  POr ps ∨ q      = POr (ps ++ [q])
  p      ∨ POr qs = POr (p:qs)
  p      ∨ q      = POr [p,q]

instance BoundedJoinSemilattice Pred where
  bot = PFalse

instance Uniplate Pred where
  uniplate = \case
    PAnd ps       -> plate PAnd ||* ps
    POr ps        -> plate POr ||* ps
    PImpl p q     -> plate PImpl |* p |* q
    PIff p q      -> plate PIff |* p |* q
    PNot p        -> plate PNot |* p
    PExists x b p -> plate PExists |- x |- b |* p
    PRel p       -> plate PRel |- p
    PTrue         -> plate PTrue
    PFalse        -> plate PFalse
    PAppK k ys    -> plate PAppK |- k |- ys

instance Biplate Pred Expr where
  biplate = \case
    PAnd ps       -> plate PAnd ||+ ps
    POr ps        -> plate POr ||+ ps
    PImpl p q     -> plate PImpl |+ p |+ q
    PIff p q      -> plate PIff |+ p |+ q
    PNot p        -> plate PNot |+ p
    PExists x b p -> plate PExists |- x |- b |+ p
    PRel p       -> plate PRel |+ p    
    PTrue         -> plate PTrue
    PFalse        -> plate PFalse
    PAppK k ys    -> plate PAppK |- k |- ys

instance Pretty Pred where
  pretty p0 = case p0 of
    PAppK k xs -> ann Highlight $ prettyKVarName k <> prettyTuple xs
    PNot p1 -> symNeg <> parensIf (p1 `needsParensPrefixedBy` p0) (pretty p1)
    PIff   p1 p2 -> prettyL p0 p1 <+> symIff     <+> prettyR p0 p2
    PImpl  p1 p2 -> prettyL p0 p1 <+> symImplies <+> prettyR p0 p2
    PAnd ps -> concatWithOp wedge $ map (prettyL p0) ps
    POr  ps -> concatWithOp vee  $ map (prettyL p0) ps
    PExists x b p -> parens $ 
      symExists <> pretty x <> colon <> pretty b <> dot <+> pretty p    
    PTrue  -> symTrue
    PFalse -> symFalse
    PRel p -> pretty p

instance HasFixity Pred where
  fixity = \case
    PNot _    -> Prefix
    PRel _    -> Infix NoAss 4
    PAnd _    -> Infix NoAss 3
    POr _     -> Infix NoAss 3
    PImpl _ _ -> Infix NoAss 1
    PIff _ _  -> Infix NoAss 1
    _         -> Infix LeftAss 9

mkBoolPred :: Bool -> Pred
mkBoolPred True = PTrue
mkBoolPred False = PFalse

pEq :: Expr -> Expr -> Pred
pEq a b = PRel (Rel Eq a b)
