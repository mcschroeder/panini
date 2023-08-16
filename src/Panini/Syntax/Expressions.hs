module Panini.Syntax.Expressions where

import Algebra.Lattice
import Data.Generics.Uniplate.Direct
import Data.Hashable
import Data.Text (Text)
import GHC.Generics (Generic)
import Panini.Abstract.AValue
import Panini.Pretty
import Panini.Provenance
import Panini.Syntax.Names
import Panini.Syntax.Primitives
import Prelude

------------------------------------------------------------------------------

-- | Expressions within predicates are built from constants, variables, linear
-- integer arithmetic, functions over strings, and uninterpreted functions.
data Expr  
  = EVal Value              -- ^ constant @c@ or variable @x@
  | EAbs AValue             -- ^ abstract value @α@
  | ENot Expr               -- ^ Boolean negation @¬e@
  | EAdd Expr Expr          -- ^ integer addition @e₁ + e₂@
  | ESub Expr Expr          -- ^ integer subtraction @e₁ - e₂@
  | EMul Expr Expr          -- ^ integer multiplication @e₁ * e₂@
  | EStrLen Expr            -- ^ string length @|s|@
  | EStrAt Expr Expr        -- ^ character at index @s[i]@
  | EStrSub Expr Expr Expr  -- ^ substring @s[i..j]@ (inclusive bounds)
  | EFun Name [Expr]        -- ^ uninterpreted function @f(e₁,e₂,…,eₙ)@   
  deriving stock (Eq, Show, Read, Generic)

instance Hashable Expr

pattern EVar :: Name -> Expr
pattern EVar x = EVal (Var x)

pattern ECon :: Constant -> Expr
pattern ECon c = EVal (Con c)

pattern EBool :: Bool -> PV -> Expr
pattern EBool b pv = ECon (B b pv)

pattern EInt :: Integer -> PV -> Expr
pattern EInt i pv = ECon (I i pv)

pattern EStr :: Text -> PV -> Expr
pattern EStr s pv = ECon (S s pv)

pattern EBoolA :: ABool -> Expr
pattern EBoolA a = EAbs (ABool a)

pattern EIntA :: AInt -> Expr
pattern EIntA a = EAbs (AInt a)

pattern EStrA :: AString -> Expr
pattern EStrA a = EAbs (AString a)

pattern (:+:) :: Expr -> Expr -> Expr
pattern e1 :+: e2 = EAdd e1 e2

pattern (:-:) :: Expr -> Expr -> Expr
pattern e1 :-: e2 = ESub e1 e2

typeOfExpr :: Expr -> Maybe Base
typeOfExpr = \case
  EVal v        -> typeOfValue v
  EAbs a        -> Just $ typeOfAValue a
  ENot _        -> Just TBool
  EAdd _ _      -> Just TInt
  ESub _ _      -> Just TInt
  EMul _ _      -> Just TInt
  EStrLen _     -> Just TInt
  EStrAt _ _    -> Just TString
  EStrSub _ _ _ -> Just TString
  EFun _ _      -> Nothing

eqTypeAE :: AValue -> Expr -> Bool
eqTypeAE a e =  maybe True (typeOfAValue a ==) (typeOfExpr e)
-- note: we assume that variables are always of the right type
-- TODO: have vars track their types
-- TODO: add a predicate typechecking pass

-- TODO: allow more expression meets
instance PartialMeetSemilattice Expr where  
  EAbs a ∧? EAbs b = EAbs <$> a ∧? b
    
  EAbs a ∧? e
    | containsTop a, eqTypeAE a e = Just e
    | containsBot a, eqTypeAE a e = Just $ EAbs $ fillBot a

  e ∧? EAbs a
    | containsTop a, eqTypeAE a e = Just e
    | containsBot a, eqTypeAE a e = Just $ EAbs $ fillBot a

  a ∧? b | a == b    = Just a
         | otherwise = Nothing

instance Uniplate Expr where
  uniplate = \case
    EVal v           -> plate EVal |- v
    EAbs a           -> plate EAbs |- a
    ENot e           -> plate ENot |* e
    EAdd e1 e2       -> plate EAdd |* e1 |* e2
    ESub e1 e2       -> plate ESub |* e1 |* e2
    EMul e1 e2       -> plate EMul |* e1 |* e2
    EStrLen e1       -> plate EStrLen |* e1
    EStrAt e1 e2     -> plate EStrAt  |* e1 |* e2
    EStrSub e1 e2 e3 -> plate EStrSub |* e1 |* e2 |* e3
    EFun f es        -> plate EFun |- f ||* es

instance Biplate Expr Value where
  biplate = \case
    EVal v           -> plate EVal |* v
    EAbs a           -> plate EAbs |- a
    ENot e           -> plate ENot |+ e
    EAdd e1 e2       -> plate EAdd |+ e1 |+ e2
    ESub e1 e2       -> plate ESub |+ e1 |+ e2
    EMul e1 e2       -> plate EMul |+ e1 |+ e2
    EStrLen e1       -> plate EStrLen |+ e1
    EStrAt e1 e2     -> plate EStrAt  |+ e1 |+ e2
    EStrSub e1 e2 e3 -> plate EStrSub |+ e1 |+ e2 |+ e3
    EFun f es        -> plate EFun |- f ||+ es

instance Pretty Expr where
  pretty p0 = case p0 of
    EVal v -> pretty v
    EFun f ps -> pretty f <> prettyTuple ps
    EMul p1 p2 -> prettyL p0 p1 <+> "*" <+> prettyR p0 p2
    EAdd p1 p2 -> prettyL p0 p1 <+> "+" <+> prettyR p0 p2
    ESub p1 p2 -> prettyL p0 p1 <+> "-" <+> prettyR p0 p2
    EStrLen p -> "|" <> pretty p <> "|"
    EStrAt p1 p2 -> pretty p1 <> "[" <> pretty p2 <> "]"
    EStrSub p1 p2 p3 -> 
      pretty p1 <> "[" <> pretty p2 <> ".." <> pretty p3 <> "]"
    ENot e -> symNeg <> parens (pretty e)
    EAbs a -> pretty a

instance HasFixity Expr where
  fixity (EMul _ _) = Infix LeftAss 6
  fixity (EAdd _ _) = Infix LeftAss 5
  fixity (ESub _ _) = Infix LeftAss 5
  fixity _          = Infix LeftAss 9
