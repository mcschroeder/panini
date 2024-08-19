{-# LANGUAGE OverloadedLists #-}
module Panini.Syntax.Predicates where

import Algebra.Lattice
import Data.Data (Data)
import Data.Generics.Uniplate.Direct
import Data.Hashable
import Data.Set ((\\))
import GHC.Generics (Generic)
import Panini.Pretty
import Panini.Syntax.Expressions
import Panini.Syntax.KVar
import Panini.Syntax.Names
import Panini.Syntax.Primitives
import Panini.Syntax.Substitution
import Prelude

------------------------------------------------------------------------------

-- | Predicates are terms in a Boolean logic.
data Pred
  = PTrue
  | PFalse
  | PAnd ![Pred]               -- ^ conjunction @p₁ ∧ p₂ ∧ … ∧ pₙ@
  | POr ![Pred]                -- ^ disjunction @p₁ ∨ p₂ ∨ … ∨ pₙ@
  | PImpl !Pred !Pred          -- ^ implication @p₁ ⟹ p₂@
  | PIff !Pred !Pred           -- ^ if-and-only-if @p₁ ⟺ p₂@
  | PNot !Pred                 -- ^ negation @¬p@
  | PRel !Rel                  -- ^ relation @e₁ ⋈ e₂@
  | PAppK !KVar ![Expr]        -- ^ κ-variable application @κᵢ(y₁,y₂,…,yₙ)@  
  | PExists !Name !Base !Pred  -- ^ existential quantification @∃x:b. p@
  deriving stock 
    ( Eq
    , Ord -- ^ structural ordering
    , Show, Read
    , Generic, Data
    )

instance Hashable Pred

-- | Same as structural ordering, except that 'PTrue' and 'PFalse' are always
-- the largest and smallest element, respectively.
instance PartialOrder Pred where
  _      ⊑ PTrue  = True
  PTrue  ⊑ _      = False
  PFalse ⊑ _      = True
  _      ⊑ PFalse = False
  a      ⊑ b      = a <= b

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
    PTrue         -> plate PTrue
    PFalse        -> plate PFalse
    PAnd ps       -> plate PAnd ||* ps
    POr ps        -> plate POr ||* ps
    PImpl p q     -> plate PImpl |* p |* q
    PIff p q      -> plate PIff |* p |* q
    PNot p        -> plate PNot |* p
    PRel r        -> plate PRel |- r
    PAppK k ys    -> plate PAppK |- k |- ys
    PExists x b p -> plate PExists |- x |- b |* p

instance Biplate Pred Expr where
  biplate = \case
    PTrue         -> plate PTrue
    PFalse        -> plate PFalse
    PAnd ps       -> plate PAnd ||+ ps
    POr ps        -> plate POr ||+ ps
    PImpl p q     -> plate PImpl |+ p |+ q
    PIff p q      -> plate PIff |+ p |+ q
    PNot p        -> plate PNot |+ p
    PRel r        -> plate PRel |+ r
    PAppK k ys    -> plate PAppK |- k |- ys
    PExists x b p -> plate PExists |- x |- b |+ p
    
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

-- see Panini.Syntax.Substitution
instance Subable Pred Expr where
  subst x y = \case
    PExists n b p
      | n == y       -> PExists n b            p   -- (1)
      | n `freeIn` x -> PExists ṅ b (subst x y ṗ)  -- (2)
      | otherwise    -> PExists n b (subst x y p)  -- (3)
      where
        ṗ = subst (EVar ṅ) n p
        ṅ = freshName n ([y] <> freeVars p <> freeVars x)

    PAppK k xs -> PAppK k (map (subst x y) xs)
    PRel r     -> PRel (subst x y r)
    p          -> descend (subst x y) p

  freeVars = \case
    PTrue         -> []
    PFalse        -> []
    PAnd ps       -> mconcat $ map freeVars ps
    POr ps        -> mconcat $ map freeVars ps
    PImpl p1 p2   -> freeVars p1 <> freeVars p2
    PIff p1 p2    -> freeVars p1 <> freeVars p2
    PNot p        -> freeVars p
    PRel r        -> freeVars r
    PAppK _ xs    -> mconcat $ map freeVars xs
    PExists x _ p -> freeVars p \\ [x]
