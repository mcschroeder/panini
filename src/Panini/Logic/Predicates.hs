module Panini.Logic.Predicates where

import Data.Generics.Uniplate.Direct
import Data.Hashable
import Data.String
import GHC.Generics (Generic)
import Panini.Abstract.AValue
import Panini.Algebra.Lattice
import Panini.Names
import Panini.Pretty.Printer
import Panini.Primitives
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
  | PPred Pred2             -- ^ predicate
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
  (⊤) = PTrue

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
  (⊥) = PFalse

instance Uniplate Pred where
  uniplate = \case
    PAnd ps       -> plate PAnd ||* ps
    POr ps        -> plate POr ||* ps
    PImpl p q     -> plate PImpl |* p |* q
    PIff p q      -> plate PIff |* p |* q
    PNot p        -> plate PNot |* p
    PExists x b p -> plate PExists |- x |- b |* p
    PPred p       -> plate PPred |- p
    PTrue         -> plate PTrue
    PFalse        -> plate PFalse
    PAppK k ys    -> plate PAppK |- k |- ys

instance Biplate Pred PExpr where
  biplate = \case
    PAnd ps       -> plate PAnd ||+ ps
    POr ps        -> plate POr ||+ ps
    PImpl p q     -> plate PImpl |+ p |+ q
    PIff p q      -> plate PIff |+ p |+ q
    PNot p        -> plate PNot |+ p
    PExists x b p -> plate PExists |- x |- b |+ p
    PPred p       -> plate PPred |+ p    
    PTrue         -> plate PTrue
    PFalse        -> plate PFalse
    PAppK k ys    -> plate PAppK |- k |- ys

instance Pretty Pred where
  pretty p0 = case p0 of
    PAppK k xs -> highlight $ pretty k <> prettyTuple xs
    PNot p1 -> symNeg <> parensIf (p1 `needsParensPrefixedBy` p0) (pretty p1)
    PIff   p1 p2 -> prettyL p0 p1 <+> symIff     <+> prettyR p0 p2
    PImpl  p1 p2 -> prettyL p0 p1 <+> symImplies <+> prettyR p0 p2
    PAnd ps -> concatWithOp symAnd $ map (prettyL p0) ps
    POr  ps -> concatWithOp symOr  $ map (prettyL p0) ps
    PExists x b p -> parens $ 
      symExists <> pretty x <> symColon <> pretty b <> symDot <+> pretty p    
    PTrue  -> "true"
    PFalse -> "false"
    PPred p -> pretty p

instance HasFixity Pred where
  fixity (PNot _)       = Prefix
  fixity (PPred _)      = Infix NoAss 4
  fixity (PAnd _)       = Infix NoAss 3
  fixity (POr _)        = Infix NoAss 3
  fixity (PImpl _ _)    = Infix NoAss 1
  fixity (PIff _ _)     = Infix NoAss 1
  fixity _              = Infix LeftAss 9

------------------------------------------------------------------------------

-- TODO: rename
data Pred2
  = PRel Rel PExpr PExpr    -- ^ binary relation @e₁ R e₂@
  
  -- TODO: replace with proper RE type
  | PReg Value String       -- ^ regular language membership @v ∈ RE@
  deriving stock (Eq, Show, Read, Generic)

instance Hashable Pred2

instance Biplate Pred2 PExpr where
  biplate = \case
    PRel r e1 e2  -> plate PRel |- r |* e1 |* e2
    PReg v re -> plate PReg |- v |- re

instance Pretty Pred2 where
  pretty p0 = case p0 of
    PRel r p1 p2 -> prettyL p0 p1 <+> pretty r   <+> prettyR p0 p2
    PReg v re -> pretty v <+> "∈" <+> pretty re  -- TODO: symQuery/symIn

instance HasFixity Pred2 where
  fixity _ = Infix NoAss 4

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

instance Pretty Rel where
  pretty = \case
    Ne -> symNe
    Eq -> symEq
    Le -> symLe
    Lt -> symLt
    Ge -> symGe
    Gt -> symGt

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
pEq a b = PPred (PRel Eq a b)

evalRel :: Ord a => Rel -> (a -> a -> Bool)
evalRel = \case
  Eq -> (==)
  Ne -> (/=)
  Ge -> (>=)
  Le -> (<=)
  Gt -> (>)
  Lt -> (<)

mkBoolPred :: Bool -> Pred
mkBoolPred True = PTrue
mkBoolPred False = PFalse

------------------------------------------------------------------------------

-- | Expressions within predicates are built from constants, variables, linear
-- integer arithmetic, functions over strings, and uninterpreted functions.
data PExpr  
  = PVal Value                 -- ^ constant @c@ or variable @x@  
  | PAdd PExpr PExpr           -- ^ integer addition @e₁ + e₂@
  | PSub PExpr PExpr           -- ^ integer subtraction @e₁ - e₂@
  | PMul PExpr PExpr           -- ^ integer multiplication @e₁ * e₂@
  | PStrLen PExpr              -- ^ string length @|s|@
  | PStrAt PExpr PExpr         -- ^ character at index @s[i]@
  | PStrSub PExpr PExpr PExpr  -- ^ substring @s[i..j]@ (inclusive bounds)
  | PFun Name [PExpr]          -- ^ uninterpreted function @f(e₁,e₂,…,eₙ)@

  | PNot2 PExpr -- TODO: hack
  | PAbs AValue -- TODO
  deriving stock (Eq, Show, Read, Generic)

instance Hashable PExpr

pattern PVar :: Name -> PExpr
pattern PVar x = PVal (Var x)

pattern PCon :: Constant -> PExpr
pattern PCon c = PVal (Con c)

-- TODO: allow more expression meets
instance PartialMeetSemilattice PExpr where  
  PAbs a ∧? PAbs b = PAbs <$> a ∧? b
  
   -- TODO: Is this correct?
  PAbs (ABool   a) ∧? e | a == (⊤) = Just e
  PAbs (AInt    a) ∧? e | a == (⊤) = Just e
  PAbs (AString a) ∧? e | a == (⊤) = Just e

  a ∧? b | a == b    = Just a
         | otherwise = Nothing

instance Uniplate PExpr where
  uniplate = \case
    PAdd e1 e2       -> plate PAdd |* e1 |* e2
    PSub e1 e2       -> plate PSub |* e1 |* e2
    PMul e1 e2       -> plate PMul |* e1 |* e2
    PStrLen e1       -> plate PStrLen |* e1
    PStrAt e1 e2     -> plate PStrAt |* e1 |* e2
    PStrSub e1 e2 e3 -> plate PStrSub |* e1 |* e2 |* e3
    PFun f es        -> plate PFun |- f ||* es
    
    --TODO
    PNot2 e -> plate PNot2 |* e
    PAbs a -> plate PAbs |- a
    PVal v -> plate PVal |- v

instance Pretty PExpr where
  pretty p0 = case p0 of
    PVal v -> pretty v
    PFun f ps -> pretty f <> prettyTuple ps
    PMul p1 p2 -> prettyL p0 p1 <+> "*" <+> prettyR p0 p2
    PAdd p1 p2 -> prettyL p0 p1 <+> "+" <+> prettyR p0 p2
    PSub p1 p2 -> prettyL p0 p1 <+> "-" <+> prettyR p0 p2
    PStrLen p -> "|" <> pretty p <> "|"
    PStrAt p1 p2 -> pretty p1 <> "[" <> pretty p2 <> "]"
    PStrSub p1 p2 p3 -> 
      pretty p1 <> "[" <> pretty p2 <> symDotDot <> pretty p3 <> "]"
    
    -- TODO
    PNot2 e -> symNeg <> parens (pretty e)
    PAbs a -> pretty a

instance HasFixity PExpr where
  fixity (PMul _ _) = Infix LeftAss 6
  fixity (PAdd _ _) = Infix LeftAss 5
  fixity (PSub _ _) = Infix LeftAss 5
  fixity _          = Infix LeftAss 9

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
  deriving stock (Ord, Eq, Show, Read, Generic)

instance Hashable KVar

-- TODO: ensure uniqueness
-- | The parameters of a κ-variable.
kparams :: KVar -> [Name]
kparams (KVar _ ts) = [fromString $ "z" ++ show @Int i | i <- [0..length ts]]

instance Pretty KVar where
  pretty (KVar i _) = identifier VarIdent $ symKappa <> subscript i
