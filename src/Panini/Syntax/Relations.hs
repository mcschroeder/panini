{-# LANGUAGE UndecidableInstances #-}
module Panini.Syntax.Relations where

import Control.Applicative
import Data.Data (Data)
import Data.Generics.Uniplate.Direct
import Data.Hashable
import GHC.Generics (Generic)
import Panini.Pretty
import Panini.Syntax.Expressions
import Panini.Syntax.Names
import Panini.Syntax.Primitives
import Panini.Syntax.Substitution
import Prelude

------------------------------------------------------------------------------

type Rel = Rel' Value

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

instance Uniplate (Expr' a) => Biplate (Rel' a) (Expr' a) where
  biplate (Rel op a b) = plate Rel |- op |* a |* b

instance Biplate Rel Value where
  biplate (Rel op a b) = plate Rel |- op |+ a |+ b

instance Subable Rel Expr where
  subst x y = descendBi (subst @Expr x y)
  freeVars = mconcat . map (freeVars @Expr) . childrenBi

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
