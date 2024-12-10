{-# LANGUAGE OverloadedLists #-}
module Panini.Syntax.Expressions where

import Algebra.Lattice
import Control.Applicative
import Data.Data (Data)
import Data.Foldable
import Data.Generics.Uniplate.Direct
import Data.Hashable
import Data.List qualified as List
import Data.Set ((\\))
import Data.Text (Text)
import GHC.Generics (Generic)
import Panini.Abstract.ABool as ABool
import Panini.Abstract.AChar as AChar
import Panini.Abstract.AString as AString
import Panini.Abstract.AUnit as AUnit
import Panini.Abstract.AInt (AInt)
--import Panini.Abstract.AValue as AValue
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

data AValue
  = AUnit !AUnit
  | ABool !ABool
  | AInt !AInt
  | AChar !AChar
  | AString !AString
  | ARel !Name !Base !RelA
  deriving stock 
    ( Eq
    , Ord  -- ^ structural ordering
    , Show, Read
    , Generic, Data)

instance Hashable AValue

instance Uniplate AValue where
  uniplate = plate

instance Biplate AValue RelA where
  biplate = \case
    AUnit a     -> plate AUnit |- a
    ABool a     -> plate ABool |- a
    AInt a     -> plate AInt |- a
    AChar a     -> plate AChar |- a
    AString a     -> plate AString |- a
    ARel x b r  -> plate ARel |- x |- b |* r

instance Biplate AValue ExprA where
  biplate = \case
    AUnit a     -> plate AUnit |- a
    ABool a     -> plate ABool |- a
    AInt a     -> plate AInt |- a
    AChar a     -> plate AChar |- a
    AString a     -> plate AString |- a
    ARel x b r  -> plate ARel |- x |- b |+ r

instance Pretty AValue where
  pretty = \case
    AUnit   a -> pretty a
    ABool   a -> pretty a
    AInt    a -> pretty a
    AChar   a -> pretty a
    AString a -> pretty a
    ARel x _ r -> braces $ pretty x <> "|" <> pretty r

typeOfAValue :: AValue -> Base
typeOfAValue = \case
  AUnit   _ -> TUnit
  ABool   _ -> TBool
  AInt    _ -> TInt
  AChar   _ -> TChar
  AString _ -> TString
  ARel _ b _ -> b



------------------------------------------------------------------------------

type Expr  = Expr' Value
type ExprA = Expr' AValue

-- | Expressions within predicates are built from constants, variables, linear
-- integer arithmetic, functions over strings, and uninterpreted functions.
data Expr' a
  = EVar !Name               -- ^ variable @x@  
  | EFun !Name ![Expr' a]    -- ^ (uninterpreted) function @f(e₁,e₂,…,eₙ)@
  | EVal !a
  | EReg !ERE                -- ^ regular expression @RE@
  deriving stock (Eq, Ord, Show, Read, Generic, Data)

instance Hashable a => Hashable (Expr' a)

-- ^ abstract solution @⟨x|r⟩@
pattern ESol :: Name -> Base -> RelA -> ExprA
pattern ESol x b r = EAbs (ARel x b r)

-- ^ abstract value @α@
pattern EAbs :: AValue -> ExprA
pattern EAbs v = EVal v

-- ^ concrete constant value @c@
pattern ECon :: Value -> Expr
pattern ECon v = EVal v

{-# COMPLETE EVar, EFun, ECon, EReg #-}
{-# COMPLETE EVar, EFun, EAbs #-}

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

-- | abstract unit constant
pattern EUnitA :: AUnit -> ExprA
pattern EUnitA a = EAbs (AUnit a)

-- | abstract Boolean constant
pattern EBoolA :: ABool -> ExprA
pattern EBoolA a = EAbs (ABool a)

-- | abstract integer constant
pattern EIntA :: AInt -> ExprA
pattern EIntA a = EAbs (AInt a)

-- | abstract character constant
pattern ECharA :: AChar -> ExprA
pattern ECharA a = EAbs (AChar a)

-- | abstract string constant
pattern EStrA :: AString -> ExprA
pattern EStrA a = EAbs (AString a)

------------------------------------------------------------------------------

-- | An expression is /ground/ if it contains no variables anywhere, including
-- inside abstract values.
ground :: Uniplate (Expr' a) => Expr' a -> Bool
ground e = and [False | EVar _ <- universe e]

-- | Postfix operator for 'ground'.
(⏚) :: Uniplate (Expr' a) => Expr' a -> Bool
(⏚) = ground

-- | The abstract maximum element for the given type.
topExpr :: Base -> ExprA
topExpr TUnit   = EUnitA top
topExpr TBool   = EBoolA top
topExpr TInt    = EIntA top
topExpr TChar   = ECharA top
topExpr TString = EStrA top

-- | The abstract minimum element for the given type.
botExpr :: Base -> ExprA
botExpr TUnit   = EUnitA bot
botExpr TBool   = EBoolA bot
botExpr TInt    = EIntA bot
botExpr TChar   = ECharA bot
botExpr TString = EStrA bot

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

-- | The type of the given expression, if locally discernible.
typeOfExprA :: ExprA -> Maybe Base
typeOfExprA = \case
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
  EFun _ es     -> asum $ map typeOfExprA es
  EReg _        -> Just TString
  EAbs a        -> Just $ typeOfAValue a

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

instance Uniplate ExprA where
  uniplate = \case
    EVar x     -> plate EVar |- x
    EFun f es  -> plate EFun |- f ||* es
    EReg r     -> plate EReg |- r
    EAbs a     -> plate EAbs |+ a

instance Biplate Expr Value where
  biplate = \case
    EVar x     -> plate EVar |- x
    EFun f es  -> plate EFun |- f ||+ es
    ECon c     -> plate ECon |* c
    EReg r     -> plate EReg |- r

instance Biplate ExprA AValue where
  biplate = \case
    EVar x     -> plate EVar |- x
    EFun f es  -> plate EFun |- f ||+ es
    EReg r     -> plate EReg |- r
    EAbs a     -> plate EAbs |* a

instance Biplate ExprA RelA where
  biplate = \case
    EVar x     -> plate EVar |- x
    EFun f es  -> plate EFun |- f ||+ es
    EReg r     -> plate EReg |- r
    EAbs a     -> plate EAbs |+ a

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

-- see Panini.Syntax.Substitution
instance Subable ExprA ExprA where
  subst x y = \case
    EVar n | y == n -> x
    ESol n b r
      | y == n       -> ESol n  b            r   -- (1)
      | n `freeIn` x -> ESol n' b (subst x y r') -- (2)
      | otherwise    -> ESol n  b (subst x y r ) -- (3)
      where
        r' = subst (EVar n') n r
        n' = freshName n ([y] <> freeVars r)
    
    e -> descend (subst x y) e

  freeVars = \case
    EVar x           -> [x]
    EFun _ es        -> mconcat (map freeVars es)
    EReg _           -> []
    ESol x _ r       -> freeVars r \\ [x]
    EAbs _           -> []

------------------------------------------------------------------------------

type Rel  = Rel' Value
type RelA = Rel' AValue

-- | Relation between expressions.
data Rel' a = Rel !Rop !(Expr' a) !(Expr' a)
  deriving stock (Eq, Ord, Show, Read, Generic, Data)

data Rop = Eq | Ne | Lt | Le | Gt | Ge | In | NotIn
  deriving stock (Eq, Ord, Show, Read, Generic, Data)

{-# COMPLETE (:=:), (:≠:), (:<:), (:≤:), (:>:), (:≥:), (:∈:), (:∉:) #-}
pattern (:=:), (:≠:), (:<:), (:≤:), (:>:), (:≥:), (:∈:), (:∉:) :: Expr' a -> Expr' a -> Rel' a
pattern a :=: b = Rel Eq a b
pattern a :≠: b = Rel Ne a b
pattern a :<: b = Rel Lt a b
pattern a :≤: b = Rel Le a b
pattern a :>: b = Rel Gt a b
pattern a :≥: b = Rel Ge a b
pattern a :∈: b = Rel In a b
pattern a :∉: b = Rel NotIn a b

instance Hashable a => Hashable (Rel' a)
instance Hashable Rop

instance Uniplate RelA where
  uniplate (Rel op a b) = plate Rel |- op |+ a |+ b

instance Uniplate (Expr' a) => Biplate (Rel' a) (Expr' a) where
  biplate (Rel op a b) = plate Rel |- op |* a |* b

instance Biplate Rel Value where
  biplate (Rel op a b) = plate Rel |- op |+ a |+ b

instance Biplate RelA AValue where
  biplate (Rel op a b) = plate Rel |- op |+ a |+ b

instance Subable Rel Expr where
  subst x y = descendBi (subst @Expr x y)
  freeVars = mconcat . map (freeVars @Expr) . childrenBi

instance Subable RelA ExprA where
  subst x y = descendBi (subst @ExprA x y)
  freeVars = mconcat . map (freeVars @ExprA) . childrenBi

instance Pretty a => Pretty (Rel' a) where
  pretty (Rel op a b) = pretty a <+> pretty op <+> pretty b

instance Pretty Rop where
  pretty = \case
    Eq -> symEq
    Ne -> symNe
    Lt -> symLt
    Le -> symLe
    Gt -> symGt
    Ge -> symGe
    In -> symIn
    NotIn -> symNotIn

------------------------------------------------------------------------------

-- | The inverse of a relation, i.e., its negation, e.g., @a > b@ to @a ≤ b@.
inverse :: Rel' a -> Rel' a
inverse = \case
  e1 :=: e2 -> e1 :≠: e2
  e1 :≠: e2 -> e1 :=: e2
  e1 :≥: e2 -> e1 :<: e2
  e1 :≤: e2 -> e1 :>: e2
  e1 :>: e2 -> e1 :≤: e2
  e1 :<: e2 -> e1 :≥: e2
  e1 :∈: e2 -> e1 :∉: e2
  e1 :∉: e2 -> e1 :∈: e2

-- | The type of a variable in a given relation, if locally discernible.
typeOfVarInRel :: Name -> Rel -> Maybe Base
typeOfVarInRel x = \case
  EVar y :=: e      | x == y -> typeOfExpr e
  e      :=: EVar y | x == y -> typeOfExpr e
  EVar y :≠: e      | x == y -> typeOfExpr e
  e      :≠: EVar y | x == y -> typeOfExpr e
  EVar y :<: _      | x == y -> Just TInt
  _      :<: EVar y | x == y -> Just TInt
  EVar y :≤: _      | x == y -> Just TInt
  _      :≤: EVar y | x == y -> Just TInt  
  EVar y :>: _      | x == y -> Just TInt
  _      :>: EVar y | x == y -> Just TInt
  EVar y :≥: _      | x == y -> Just TInt
  _      :≥: EVar y | x == y -> Just TInt  
  EVar y :∈: e      | x == y -> typeOfExpr e
  e      :∈: EVar y | x == y -> typeOfExpr e
  EVar y :∉: e      | x == y -> typeOfExpr e
  e      :∉: EVar y | x == y -> typeOfExpr e  
  Rel _ e1 e2 -> typeOfVarInExpr x e1 <|> typeOfVarInExpr x e2

-- | The type of a variable in a given relation, if locally discernible.
typeOfVarInRelA :: Name -> RelA -> Maybe Base
typeOfVarInRelA x = \case
  EVar y :=: e      | x == y -> typeOfExprA e
  e      :=: EVar y | x == y -> typeOfExprA e
  EVar y :≠: e      | x == y -> typeOfExprA e
  e      :≠: EVar y | x == y -> typeOfExprA e
  EVar y :<: _      | x == y -> Just TInt
  _      :<: EVar y | x == y -> Just TInt
  EVar y :≤: _      | x == y -> Just TInt
  _      :≤: EVar y | x == y -> Just TInt  
  EVar y :>: _      | x == y -> Just TInt
  _      :>: EVar y | x == y -> Just TInt
  EVar y :≥: _      | x == y -> Just TInt
  _      :≥: EVar y | x == y -> Just TInt  
  EVar y :∈: e      | x == y -> typeOfExprA e
  e      :∈: EVar y | x == y -> typeOfExprA e
  EVar y :∉: e      | x == y -> typeOfExprA e
  e      :∉: EVar y | x == y -> typeOfExprA e  
  Rel _ e1 e2 -> typeOfVarInExpr x e1 <|> typeOfVarInExpr x e2
