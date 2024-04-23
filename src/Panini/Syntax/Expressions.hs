{-# LANGUAGE OverloadedLists #-}
module Panini.Syntax.Expressions where

import Algebra.Lattice
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
import Panini.Abstract.AValue as AValue
import Panini.Pretty
import Panini.Provenance
import Panini.Regex.POSIX.ERE (ERE)
import Panini.Syntax.Names
import Panini.Syntax.Primitives
import Panini.Syntax.Substitution
import Prelude
import Control.Applicative

-- TODO: consider changing EStrSub to start,length encoding (like SMTLIB)?
-- TODO: simplify EReg situation

------------------------------------------------------------------------------

-- | Expressions within predicates are built from constants, variables, linear
-- integer arithmetic, functions over strings, and uninterpreted functions.
data Expr
  = EVar !Name               -- ^ variable @x@  
  | EFun !Name ![Expr]       -- ^ (uninterpreted) function @f(e₁,e₂,…,eₙ)@
  | ECon !Value              -- ^ concrete constant value @c@
  | EReg !ERE                -- ^ regular expression @RE@
  | EAbs !AValue             -- ^ abstract value @α@
  | ESol !Name !Base !Rel    -- ^ abstract solution @⟨x|r⟩@
  deriving stock (Eq, Ord, Show, Read, Generic)

instance Hashable Expr

------------------------------------------------------------------------------

-- | Boolean negation @¬@
pattern ENot :: Expr -> Expr
pattern ENot e = EFun "not" [e]

-- | integer addition
pattern (:+:) :: Expr -> Expr -> Expr
pattern a :+: b = EFun "+" [a,b]

-- | integer subtraction
pattern (:-:) :: Expr -> Expr -> Expr
pattern a :-: b = EFun "-" [a,b]

-- | integer multiplication
pattern (:*:) :: Expr -> Expr -> Expr
pattern a :*: b = EFun "*" [a,b]

-- | integer modulus
pattern EMod :: Expr -> Expr -> Expr
pattern EMod a b = EFun "mod" [a,b]

-- | string length @|s|@
pattern EStrLen :: Expr -> Expr
pattern EStrLen s = EFun "str.len" [s]

-- | character at index @s[i]@
pattern EStrAt :: Expr -> Expr -> Expr
pattern EStrAt s i = EFun "str.at" [s,i]

-- | substring @s[i..j]@ (inclusive bounds)
pattern EStrSub :: Expr -> Expr -> Expr -> Expr
pattern EStrSub s i j = EFun "str.sub" [s,i,j]

-- | string complement
pattern EStrComp :: Expr -> Expr
pattern EStrComp s = EFun "str.comp" [s]

pattern EStrFirstIndexOfChar :: Expr -> Expr -> Expr
pattern EStrFirstIndexOfChar s c = EFun "firstIndexOfChar" [s,c]

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
pattern EUnitA :: AUnit -> Expr
pattern EUnitA a = EAbs (AUnit a)

-- | abstract Boolean constant
pattern EBoolA :: ABool -> Expr
pattern EBoolA a = EAbs (ABool a)

-- | abstract integer constant
pattern EIntA :: AInt -> Expr
pattern EIntA a = EAbs (AInt a)

-- | abstract character constant
pattern ECharA :: AChar -> Expr
pattern ECharA a = EAbs (AChar a)

-- | abstract string constant
pattern EStrA :: AString -> Expr
pattern EStrA a = EAbs (AString a)

------------------------------------------------------------------------------

-- | An expression is /ground/ if it contains no variables; in particular, it
-- also does not contain any abstract solutions.
ground :: Expr -> Bool
ground (EVar _)     = False
ground (EFun _ es)  = all ground es
ground (ECon _)     = True
ground (EReg _)     = True
ground (EAbs _)     = True
ground (ESol _ _ _) = False

-- | Postfix operator for 'ground'.
(⏚) :: Expr -> Bool
(⏚) = ground

-- | An expression is /abstract/ if it involves abstract values.
isAbstract :: Expr -> Bool
isAbstract e0 = or [isAbs e | e <- universe e0]
 where
  isAbs (EAbs _)     = True
  isAbs (ESol _ _ _) = True
  isAbs (EReg _)     = True -- TODO: ?
  isAbs _            = False

-- | An expression is /concrete/ if it does not involve any abstract values.
isConcrete :: Expr -> Bool
isConcrete = not . isAbstract

-- | The abstract maximum element for the given type.
topExpr :: Base -> Expr
topExpr TUnit   = EUnitA top
topExpr TBool   = EBoolA top
topExpr TInt    = EIntA top
topExpr TChar   = ECharA top
topExpr TString = EStrA top

-- | The abstract minimum element for the given type.
botExpr :: Base -> Expr
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
  EFun _ es     -> asum $ map typeOfExpr es
  ECon c        -> Just $ typeOfValue c
  EReg _        -> Just TString
  EAbs a        -> Just $ typeOfAValue a
  ESol _ b _    -> Just b

-- | The type of a variable in a given expression, if locally discernible.
typeOfVarInExpr :: Name -> Expr -> Maybe Base
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
  EStrLen (EVar y)      | x == y -> Just TInt
  EStrAt (EVar y) _     | x == y -> Just TString
  EStrAt _ (EVar y)     | x == y -> Just TInt
  EStrSub (EVar y) _ _  | x == y -> Just TString
  EStrSub _ (EVar y) _  | x == y -> Just TInt
  EStrSub _ _ (EVar y)  | x == y -> Just TInt
  EStrFirstIndexOfChar (EVar y) _ | x == y -> Just TString
  EStrFirstIndexOfChar _ (EVar y) | x == y -> Just TChar
  ESol y _ _            | x == y -> Nothing
  ESol _ _ r                     -> typeOfVarInRel x r
  EFun _ es                      -> asum $ map (typeOfVarInExpr x) es
  EVar _                         -> Nothing
  ECon _                         -> Nothing
  EReg _                         -> Nothing
  EAbs _                         -> Nothing

------------------------------------------------------------------------------

instance Uniplate Expr where
  uniplate = \case
    EVar x     -> plate EVar |- x
    EFun f es  -> plate EFun |- f ||* es
    ECon c     -> plate ECon |- c
    EReg r     -> plate EReg |- r
    EAbs a     -> plate EAbs |- a
    ESol x b r -> plate ESol |- x |- b |+ r

instance Biplate Expr Value where
  biplate = \case
    EVar x     -> plate EVar |- x
    EFun f es  -> plate EFun |- f ||+ es
    ECon c     -> plate ECon |* c
    EReg r     -> plate EReg |- r
    EAbs a     -> plate EAbs |- a
    ESol x b r -> plate ESol |- x |- b |+ r

instance Biplate Expr AValue where
  biplate = \case
    EVar x     -> plate EVar |- x
    EFun f es  -> plate EFun |- f ||+ es
    ECon c     -> plate ECon |- c
    EReg r     -> plate EReg |- r
    EAbs a     -> plate EAbs |* a
    ESol x b r -> plate ESol |- x |- b |+ r

instance Biplate Expr Rel where
  biplate = \case
    EVar x     -> plate EVar |- x
    EFun f es  -> plate EFun |- f ||+ es
    ECon c     -> plate ECon |- c
    EReg r     -> plate EReg |- r
    EAbs a     -> plate EAbs |- a
    ESol x b r -> plate ESol |- x |- b |* r

instance Pretty Expr where
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
    ECon c        -> pretty c
    EReg r        -> pretty r
    EAbs a        -> pretty a
    ESol x _ r    -> braces $ pretty x <> "|" <> pretty r
   where
    -- TODO: make use of fixity for this
    complex (_ :*: _) = True
    complex (_ :+: _) = True
    complex (_ :-: _) = True
    complex _         = False

instance HasFixity Expr where
  fixity (_ :*: _) = Infix LeftAss 6
  fixity (_ :+: _) = Infix LeftAss 5
  fixity (_ :-: _) = Infix LeftAss 5
  fixity _         = Infix LeftAss 9

-- see Panini.Syntax.Substitution
instance Subable Expr Expr where
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
    ECon _           -> []
    EReg _           -> []
    EAbs _           -> []
    ESol x _ r       -> freeVars r \\ [x]

------------------------------------------------------------------------------

-- | Relation between expressions.
data Rel = Rel !Rop !Expr !Expr
  deriving stock (Eq, Ord, Show, Read, Generic)

data Rop = Eq | Ne | Lt | Le | Gt | Ge | In | NotIn
  deriving stock (Eq, Ord, Show, Read, Generic)

{-# COMPLETE (:=:), (:≠:), (:<:), (:≤:), (:>:), (:≥:), (:∈:), (:∉:) #-}
pattern (:=:), (:≠:), (:<:), (:≤:), (:>:), (:≥:), (:∈:), (:∉:) :: Expr -> Expr -> Rel
pattern a :=: b = Rel Eq a b
pattern a :≠: b = Rel Ne a b
pattern a :<: b = Rel Lt a b
pattern a :≤: b = Rel Le a b
pattern a :>: b = Rel Gt a b
pattern a :≥: b = Rel Ge a b
pattern a :∈: b = Rel In a b
pattern a :∉: b = Rel NotIn a b

instance Hashable Rel
instance Hashable Rop

instance Uniplate Rel where
  uniplate (Rel op a b) = plate Rel |- op |+ a |+ b

instance Biplate Rel Expr where
  biplate (Rel op a b) = plate Rel |- op |* a |* b

instance Biplate Rel Value where
  biplate (Rel op a b) = plate Rel |- op |+ a |+ b

instance Biplate Rel AValue where
  biplate (Rel op a b) = plate Rel |- op |+ a |+ b

instance Subable Rel Expr where
  subst x y = descendBi (subst @Expr x y)
  freeVars = mconcat . map (freeVars @Expr) . childrenBi

instance Pretty Rel where
  pretty (Rel op a b) = pretty a <+> pop <+> pretty b
   where
    pop = case (op, isAbstract a, isAbstract b) of
      (Eq, True , True ) -> symNei
      (Eq, True , False) -> symNi
      (Eq, False, True ) -> symIn
      (Eq, False, False) -> symEq
      (Ne, True , True ) -> symEi
      (Ne, True , False) -> symNotNi
      (Ne, False, True ) -> symNotIn
      (Ne, False, False) -> symNe
      _                  -> pretty op

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
inverse :: Rel -> Rel
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
