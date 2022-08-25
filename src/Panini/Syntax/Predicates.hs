module Panini.Syntax.Predicates where

import Data.Hashable
import Data.String
import GHC.Generics
import Panini.Syntax.Names
import Panini.Syntax.Primitives
import Prelude

------------------------------------------------------------------------------

-- | Predicates are terms in a Boolean logic.
data Pred
  = PTrue
  | PFalse
  | PRel Rel PExpr PExpr    -- ^ binary relation @e₁ R e₂@
  | PAnd [Pred]             -- ^ conjunction @p₁ ∧ p₂ ∧ … ∧ pₙ@
  | POr [Pred]              -- ^ disjunction @p₁ ∧ p₂ ∧ … ∧ pₙ@
  | PImpl Pred Pred         -- ^ implication @p₁ ⟹ p₂@
  | PIff Pred Pred          -- ^ if-and-onlyy-if @p₁ ⟺ p₂@
  | PNot Pred               -- ^ negation @¬p@
  | PAppK KVar [Name]       -- ^ κ-variable application @κᵢ(y₁,y₂,…,yₙ)@  
  | PExists Name Base Pred  -- ^ existential quantification @∃x:b. p@
  deriving stock (Eq, Show, Read)

-- | Smart constructor for `PAnd`, eliminates redundant values and merges
-- adjacent `PAnd` lists.
pAnd :: Pred -> Pred -> Pred
pAnd PTrue       q           = q
pAnd PFalse      _           = PFalse
pAnd p           PTrue       = p
pAnd _           PFalse      = PFalse
pAnd (PAnd ps)   (PAnd qs)   = PAnd (ps ++ qs)
pAnd (PAnd ps)   q           = PAnd (ps ++ [q])
pAnd p           (PAnd qs)   = PAnd (p:qs)
pAnd p           q           = PAnd [p,q]

-- | Smart constructor for `POr`, eliminates redundant values and merges
-- adjacent `POr` lists.
pOr :: Pred -> Pred -> Pred
pOr PFalse     q          = q
pOr PTrue      _          = PTrue
pOr p          PFalse     = p
pOr _          PTrue      = PTrue
pOr (POr ps)   (POr qs)   = POr (ps ++ qs)
pOr (POr ps)   q          = POr (ps ++ [q])
pOr p          (POr qs)   = POr (p:qs)
pOr p          q          = POr [p,q]

------------------------------------------------------------------------------

-- | A relation between two expressions.
data Rel 
  = Eq  -- ^ equal @=@
  | Ne  -- ^ unequal @≠@
  | Ge  -- ^ greater than or equal @≥@
  | Le  -- ^ less than or equal @≤@
  | Gt  -- ^ greater than @>@
  | Lt  -- ^ less than @<@
  deriving stock (Eq, Ord, Generic, Show, Read)

instance Hashable Rel

-- | Inverse of a relation, e.g., ≥ to <.
invRel :: Rel -> Rel
invRel = \case
  Eq -> Ne
  Ne -> Eq
  Ge -> Lt
  Le -> Gt
  Gt -> Le
  Lt -> Ge

-- | Converse of a relation, e.g., ≥ to ≤.
convRel :: Rel -> Rel
convRel = \case
  Eq -> Eq
  Ne -> Ne
  Ge -> Le
  Le -> Ge
  Gt -> Lt
  Lt -> Gt

pEq :: PExpr -> PExpr -> Pred
pEq = PRel Eq

------------------------------------------------------------------------------

-- | Expressions within predicates are built from constants, variables, linear
-- integer arithmetic, functions over strings, and uninterpreted functions.
data PExpr  
  = PCon Constant              -- ^ constant @c@  
  | PVar Name                  -- ^ variable @x@
  | PAdd PExpr PExpr           -- ^ integer addition @e₁ + e₂@
  | PSub PExpr PExpr           -- ^ integer subtraction @e₁ - e₂@
  | PMul PExpr PExpr           -- ^ integer multiplication @e₁ * e₂@
  | PStrLen PExpr              -- ^ string length @|s|@
  | PStrAt PExpr PExpr         -- ^ character at index @s[i]@
  | PStrSub PExpr PExpr PExpr  -- ^ substring @s[i..j]@ (inclusive bounds)
  | PFun Name [PExpr]          -- ^ uninterpreted function @f(e₁,e₂,…,eₙ)@
  deriving stock (Eq, Show, Read)

------------------------------------------------------------------------------

-- | A /refinement variable/ κ represents an unknown refinement over some free
-- variables z₁,z₂,…,zₙ, known as the /parameters/ of κ. For any κ-variable, we
-- know its arity (the number of parameters) as well as the expected parameter
-- types τ₁,τ₂,…,τₙ.
-- 
-- Refinement variables can occur in predicates in the form of applications
-- ('PAppK') which bind the parameters to particular values. Note that the
-- refinement represented by an applied κ-variable is unknown until the solving
-- phase, where we aim to find some assignment σ that maps each κ-variable to a
-- concrete refinement predicate satisfying the constraints induced by the
-- applications.
--
-- In the literature, κ-variables are sometimes referred to as /Horn variables/.
data KVar = KVar Int [Base]
  deriving stock (Ord, Eq, Show, Read)

-- | The parameters of a κ-variable.
kparams :: KVar -> [Name]
kparams (KVar _ ts) = [fromString $ "z" ++ show @Int i | i <- [0..length ts]]
