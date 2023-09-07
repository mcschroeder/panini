module Panini.Syntax.Expressions where

import Algebra.Lattice
import Control.Applicative
import Data.Generics.Uniplate.Direct
import Data.Hashable
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Panini.Abstract.ABool as ABool
import Panini.Abstract.AInt as AInt
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

pattern EChar :: Char -> PV -> Expr
pattern EChar c pv <- ECon (S (Text.unpack -> [c]) pv) where
  EChar c pv = ECon (S (Text.pack [c]) pv)

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

------------------------------------------------------------------------------

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

------------------------------------------------------------------------------

-- TODO: allow more expression meets
instance PartialMeetSemilattice Expr where  
  EAbs a ∧? EAbs b = EAbs <$> a ∧? b  
  
  EBool  a _ ∧? EBool  b _ = Just $ EBoolA $ ABool.eq a ∧ ABool.eq b
  EBoolA a   ∧? EBool  b _ = Just $ EBoolA $ a          ∧ ABool.eq b
  EBool  a _ ∧? EBoolA b   = Just $ EBoolA $ ABool.eq a ∧ b

  EInt  a _ ∧? EInt  b _ = Just $ EIntA $ AInt.eq a ∧ AInt.eq b
  EIntA a   ∧? EInt  b _ = Just $ EIntA $ a         ∧ AInt.eq b
  EInt  a _ ∧? EIntA b   = Just $ EIntA $ AInt.eq a ∧ b

  a ∧? b | a == b    = Just a
         | otherwise = tryMeetE a b <|> tryMeetE b a

tryMeetE :: Expr -> Expr -> Maybe Expr
tryMeetE (EAbs a) e
  | containsTop a, eqTypeAE a e = Just e
  | containsBot a, eqTypeAE a e = Just $ EAbs $ fillBot a
  
tryMeetE (EVar x) (EVar y :+: EAbs (AInt a))
  | x == y, AInt.concreteMember 0 a = Just $ EVar x
  | x == y                          = Just $ EAbs (AInt bot)
  
tryMeetE (EStrLen (EVar s1)) (EStrLen (EVar s2) :+: EIntA a)
  | s1 == s2, AInt.concreteMember 0 a = Just $ EStrLen (EVar s1)
  | s1 == s2                          = Just $ EIntA bot

tryMeetE _ _ = Nothing

------------------------------------------------------------------------------

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
