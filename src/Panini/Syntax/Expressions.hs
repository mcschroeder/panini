{-# LANGUAGE OverloadedLists #-}
module Panini.Syntax.Expressions where

import Control.Applicative
import Data.Data (Data)
import Data.Foldable
import Data.Generics.Uniplate.Direct
import Data.Hashable
import Data.List qualified as List
import Data.Text (Text)
import GHC.Generics (Generic)
import Panini.Pretty
import Panini.Provenance
import Panini.Syntax.Names
import Panini.Syntax.Primitives
import Panini.Syntax.Substitution
import Prelude
import Regex.POSIX.ERE (ERE, printERE)

-- TODO: consider changing EStrSub to start,length encoding (like SMTLIB)?
-- TODO: simplify EReg situation

------------------------------------------------------------------------------

type Expr = Expr' Value

-- | Expressions within predicates are built from constants, variables, linear
-- integer arithmetic, functions over strings, and uninterpreted functions.
data Expr' a
  = EVar !Name               -- ^ variable @x@  
  | EFun !Name ![Expr' a]    -- ^ (uninterpreted) function @f(e₁,e₂,…,eₙ)@
  | EVal !a
  | EReg !ERE                -- ^ regular expression @RE@
  deriving stock (Eq, Ord, Show, Read, Generic, Data)

instance Hashable a => Hashable (Expr' a)

-- ^ concrete constant value @c@
pattern ECon :: Value -> Expr
pattern ECon v = EVal v

{-# COMPLETE EVar, EFun, ECon, EReg #-}

-- | unit constant
pattern EUnit :: PV -> Expr
pattern EUnit pv = ECon (U pv)

-- | Boolean constant
pattern EBool :: Bool -> PV -> Expr
pattern EBool b pv = ECon (B b pv)

-- | integer constant
pattern EInt :: Integer -> PV -> Expr
pattern EInt i pv = ECon (I i pv)

-- | character constant
pattern EChar :: Char -> PV -> Expr
pattern EChar c pv = ECon (C c pv)

-- | string constant
pattern EStr :: Text -> PV -> Expr
pattern EStr s pv = ECon (S s pv)

------------------------------------------------------------------------------

-- | Boolean negation @¬@
pattern ENot :: Expr' a -> Expr' a
pattern ENot e = EFun "not" [e]

-- | integer addition
pattern (:+:) :: Expr' a -> Expr' a -> Expr' a
pattern a :+: b = EFun "+" [a,b]

-- | integer subtraction
pattern (:-:) :: Expr' a -> Expr' a -> Expr' a
pattern a :-: b = EFun "-" [a,b]

-- | integer multiplication
pattern (:*:) :: Expr' a -> Expr' a -> Expr' a
pattern a :*: b = EFun "*" [a,b]

-- | integer modulus
pattern EMod :: Expr' a -> Expr' a -> Expr' a
pattern EMod a b = EFun "mod" [a,b]

-- | string length @|s|@
pattern EStrLen :: Expr' a -> Expr' a
pattern EStrLen s = EFun "str.len" [s]

-- | character at index @s[i]@
pattern EStrAt :: Expr' a -> Expr' a -> Expr' a
pattern EStrAt s i = EFun "str.at" [s,i]

-- | substring @s[i..j]@ (inclusive bounds)
pattern EStrSub :: Expr' a -> Expr' a -> Expr' a -> Expr' a
pattern EStrSub s i j = EFun "str.sub" [s,i,j]

-- | string complement
pattern EStrComp :: Expr' a -> Expr' a
pattern EStrComp s = EFun "str.comp" [s]

pattern EStrFirstIndexOfChar :: Expr' a -> Expr' a -> Expr' a
pattern EStrFirstIndexOfChar s c = EFun "firstIndexOfChar" [s,c]

pattern EStrIndexOf :: Expr' a -> Expr' a -> Expr' a -> Expr' a
pattern EStrIndexOf s t i = EFun "str_indexof" [s,t,i]

-- | string concatenation
pattern EStrConc :: Expr' a -> Expr' a -> Expr' a
pattern EStrConc a b = EFun "str.++" [a,b]

pattern EStrStar :: Expr' a -> Expr' a
pattern EStrStar s = EFun "re_star" [s]

pattern EStrContains :: Expr' a -> Expr' a -> Expr' a
pattern EStrContains s t = EFun "str_contains" [s,t]

------------------------------------------------------------------------------

-- | An expression is /ground/ if it contains no variables anywhere, including
-- inside abstract values.
ground :: Uniplate (Expr' a) => Expr' a -> Bool
ground e = and [False | EVar _ <- universe e]

-- | Postfix operator for 'ground'.
(⏚) :: Uniplate (Expr' a) => Expr' a -> Bool
(⏚) = ground

-- | The type of the given expression, if locally discernible.
typeOfExpr :: Expr -> Maybe Base
typeOfExpr = \case
  EVar _        -> Nothing
  ENot _        -> Just TBool
  _ :+: _       -> Just TInt
  _ :-: _       -> Just TInt
  _ :*: _       -> Just TInt
  EStrLen _     -> Just TInt
  EStrAt _ _    -> Just TChar
  EStrSub _ _ _ -> Just TString
  EStrFirstIndexOfChar _ _ -> Just TInt
  EStrConc _ _ -> Just TString
  EStrStar _ -> Just TString
  EStrContains _ _ -> Just TBool
  EFun _ es     -> asum $ map typeOfExpr es
  ECon c        -> Just $ typeOfValue c
  EReg _        -> Just TString

-- | The type of a variable in a given expression, if locally discernible.
typeOfVarInExpr :: Name -> Expr' a -> Maybe Base
typeOfVarInExpr x = \case
  ENot (EVar y)         | x == y -> Just TBool
  EVar y :+: _          | x == y -> Just TInt
  _ :+: EVar y          | x == y -> Just TInt
  EVar y :-: _          | x == y -> Just TInt
  _ :-: EVar y          | x == y -> Just TInt
  EVar y :*: _          | x == y -> Just TInt
  _ :*: EVar y          | x == y -> Just TInt
  EMod (EVar y) _       | x == y -> Just TInt
  EMod _ (EVar y)       | x == y -> Just TInt
  EStrLen (EVar y)      | x == y -> Just TString
  EStrAt (EVar y) _     | x == y -> Just TString
  EStrAt _ (EVar y)     | x == y -> Just TInt
  EStrSub (EVar y) _ _  | x == y -> Just TString
  EStrSub _ (EVar y) _  | x == y -> Just TInt
  EStrSub _ _ (EVar y)  | x == y -> Just TInt
  EStrFirstIndexOfChar (EVar y) _ | x == y -> Just TString
  EStrFirstIndexOfChar _ (EVar y) | x == y -> Just TChar
  EStrStar (EVar y) | x == y -> Just TString
  EStrConc (EVar y) _ | x == y -> Just TString
  EStrConc _ (EVar y) | x == y -> Just TString
  EStrContains (EVar y) _ | x == y -> Just TString
  EStrContains _ (EVar y) | x == y -> Just TString  
  EFun _ es                      -> asum $ map (typeOfVarInExpr x) es
  EVar _                         -> Nothing
  EReg _                         -> Nothing
  EVal _                         -> Nothing

------------------------------------------------------------------------------

instance Uniplate Expr where
  uniplate = \case
    EVar x     -> plate EVar |- x
    EFun f es  -> plate EFun |- f ||* es
    ECon c     -> plate ECon |- c
    EReg r     -> plate EReg |- r


instance Biplate Expr Value where
  biplate = \case
    EVar x     -> plate EVar |- x
    EFun f es  -> plate EFun |- f ||+ es
    ECon c     -> plate ECon |* c
    EReg r     -> plate EReg |- r


instance Pretty a => Pretty (Expr' a) where
  pretty e0 = case e0 of
    EVar x        -> pretty x
    ENot e        -> symNeg <> parensIf (complex e) (pretty e)
    a :*: b       -> prettyL e0 a <+> "*" <+> prettyR e0 b
    a :+: b       -> prettyL e0 a <+> "+" <+> prettyR e0 b
    a :-: b       -> prettyL e0 a <+> "-" <+> prettyR e0 b
    EStrLen s     -> "|" <> pretty s <> "|"
    EStrAt s i    -> pretty s <> "[" <> pretty i <> "]"
    EStrSub s i j -> pretty s <> "[" <> pretty i <> ".." <> pretty j <> "]"
    EFun f es     -> pretty f <> parens (mconcat $ List.intersperse ", " $ map pretty es)
    EReg r        -> ann (Literal StringLit) $ pretty $ printERE r
    EVal a        -> pretty a
   where
    -- TODO: make use of fixity for this
    complex (_ :*: _) = True
    complex (_ :+: _) = True
    complex (_ :-: _) = True
    complex _         = False

instance HasFixity (Expr' a) where
  fixity (_ :*: _) = Infix LeftAss 6
  fixity (_ :+: _) = Infix LeftAss 5
  fixity (_ :-: _) = Infix LeftAss 5
  fixity _         = Infix LeftAss 9

-- see Panini.Syntax.Substitution
instance Subable Expr Expr where
  subst x y = \case
    EVar n | y == n -> x
    e -> descend (subst x y) e

  freeVars = \case
    EVar x           -> [x]
    EFun _ es        -> mconcat (map freeVars es)
    ECon _           -> []
    EReg _           -> []
