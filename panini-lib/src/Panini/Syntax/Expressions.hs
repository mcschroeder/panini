{-# LANGUAGE OverloadedLists #-}
module Panini.Syntax.Expressions where

import Data.Data (Data)
import Data.Foldable
import Data.Generics.Uniplate.Direct
import Data.Hashable
import Data.List qualified as List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
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
  = EVar !Name !Base         -- ^ variable @x@  
  | EFun !Name ![Expr' a]    -- ^ (uninterpreted) function @f(e₁,e₂,…,eₙ)@
  | EVal !a
  | EReg !ERE                -- ^ regular expression @RE@
  deriving stock (Eq, Ord, Functor, Show, Read, Generic, Data)

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
ground e = and [False | EVar _ _ <- universe e]

-- | Postfix operator for 'ground'.
(⏚) :: Uniplate (Expr' a) => Expr' a -> Bool
(⏚) = ground

-- | The type of the given expression, if locally discernible.
typeOfExpr :: Expr -> Maybe Base
typeOfExpr = \case
  EVar _ b      -> Just b
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
  EFun _ _      -> Nothing
  ECon c        -> Just $ typeOfValue c
  EReg _        -> Just TString

-- | The type of a variable in a given expression, if locally discernible.
typeOfVarInExpr :: Uniplate (Expr' a) => Name -> Expr' a -> Maybe Base
typeOfVarInExpr x e = listToMaybe [b | EVar y b <- universe e, y == x]

------------------------------------------------------------------------------

instance Uniplate Expr where
  uniplate = \case
    EVar x b   -> plate EVar |- x |- b
    EFun f es  -> plate EFun |- f ||* es
    ECon c     -> plate ECon |- c
    EReg r     -> plate EReg |- r


instance Biplate Expr Value where
  biplate = \case
    EVar x b   -> plate EVar |- x |- b
    EFun f es  -> plate EFun |- f ||+ es
    ECon c     -> plate ECon |* c
    EReg r     -> plate EReg |- r


instance Pretty a => Pretty (Expr' a) where
  pretty e0 = case e0 of
    EVar x _      -> pretty x
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
    EVar n _ | y == n -> x
    e -> descend (subst x y) e

  freeVars = \case
    EVar x _         -> [x]
    EFun _ es        -> mconcat (map freeVars es)
    ECon _           -> []
    EReg _           -> []

------------------------------------------------------------------------------

-- | Normalize an expression by (partial) evaluation. 
normExpr :: Expr -> Expr
normExpr = \case
  -----------------------------------------------------------
  ENot (EBool a pv)                           -> EBool (not a) pv
  ENot (ENot e)                               -> normExpr $ e
  -----------------------------------------------------------
  EInt  a _ :+: EInt  b _                     -> EInt (a + b) NoPV
  a         :+: EInt  0 _                     -> normExpr $ a
  a         :+: EInt  b pv | b <= 0           -> normExpr $ a :-: EInt (negate b) pv
  a         :+: b          | a > b            -> normExpr $ b :+: a
  (a :+: b) :+: c          | (b ⏚), (c ⏚)    -> normExpr $ a :+: (normExpr $ b :+: c)
  (a :-: b) :+: c          | (b ⏚), (c ⏚)    -> normExpr $ a :-: (normExpr $ b :-: c)
  -----------------------------------------------------------
  EInt  a _ :-: EInt  b _                     -> EInt (a - b) NoPV
  a         :-: EInt  0 _                     -> normExpr $ a
  a         :-: EInt  b pv | b <= 0           -> normExpr $ a :+: EInt (negate b) pv
  (a :-: b) :-: c          | (b ⏚), (c ⏚)    -> normExpr $ a :-: (normExpr $ b :+: c)
  (a :+: b) :-: c          | (b ⏚), (c ⏚)    -> normExpr $ a :+: (normExpr $ b :-: c)
  (a :+: b) :-: c          | (a ⏚), (c ⏚)    -> normExpr $ b :+: (normExpr $ a :-: c)
  -----------------------------------------------------------
  EMod (EInt a _) (EInt b _)                  -> EInt (a `mod` b) NoPV
  -----------------------------------------------------------
  EStrLen (EStr s _)                          -> EInt (fromIntegral $ Text.length s) NoPV
  -----------------------------------------------------------
  EStrAt (EStr s _) (EInt (fromInteger -> i) _) | i < Text.length s -> normExpr $ EChar (Text.index s i) NoPV
  -----------------------------------------------------------
  EStrSub (EStr s _) (EInt (fromInteger -> i) _) (EInt (fromInteger -> j) _) | i >= 0, i <= j, j <= Text.length s -> normExpr $ EStr (Text.take (j - i + 1) $ Text.drop i s) NoPV
  EStrSub s1 (EInt 0 _) (EStrLen s2 :-: EInt 1 _) | s1 == s2 -> normExpr s1
  -----------------------------------------------------------
  EStrComp (EStrComp e)                       -> normExpr $ e  
  -----------------------------------------------------------
  EStrConc (EStr  a _) (EStr  b _)            -> normExpr $ EStr (a <> b) NoPV
  -----------------------------------------------------------
  EStrConc (EStrSub s1 (EInt i1 pvi1) (EInt j1 _)) (EStrSub s2 (EInt i2 _) (EInt j2 pvj2))
    | s1 == s2, i1 <= j1, j1 + 1 == i2, i2 <= j2
    -> normExpr $ EStrSub s1 (EInt i1 pvi1) (EInt j2 pvj2)
  -----------------------------------------------------------
  EStrContains (EStr s _) (EStr t _)          -> EBool (t `Text.isInfixOf` s) NoPV
  -----------------------------------------------------------
  e | e' <- descend normExpr e, e' /= e       -> normExpr e'
    | otherwise                               -> e