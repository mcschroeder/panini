module Panini.Logic.Expressions where

import Data.Generics.Uniplate.Direct
import Data.Hashable
import GHC.Generics (Generic)
import Panini.Abstract.AValue
import Panini.Algebra.Lattice
import Panini.Names
import Panini.Pretty.Printer
import Panini.Primitives
import Prelude

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
  PAbs (ABool   a) ∧? e | isTop a = Just e
  PAbs (AInt    a) ∧? e | isTop a = Just e
  PAbs (AString a) ∧? e | isTop a = Just e

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
