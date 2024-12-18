{-# LANGUAGE UndecidableInstances #-}
module Panini.Syntax.Relations where

import Data.Data (Data)
import Data.Generics.Uniplate.Direct
import Data.Hashable
import Data.Maybe
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Panini.Pretty
import Panini.Provenance
import Panini.Syntax.Expressions
import Panini.Syntax.Names
import Panini.Syntax.Primitives
import Panini.Syntax.Substitution
import Prelude

------------------------------------------------------------------------------

type Rel = Rel' Value

-- | Relation between expressions.
data Rel' a = Rel !Rop !(Expr' a) !(Expr' a)
  deriving stock (Eq, Ord, Functor, Show, Read, Generic, Data)

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

instance Biplate Rel Expr where
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
typeOfVarInRel :: forall a. Biplate (Rel' a) (Expr' a) => Name -> Rel' a -> Maybe Base
typeOfVarInRel x r = 
  listToMaybe [b | EVar y b <- universeBi @(Rel' a) @(Expr' a) r, x == y]

-------------------------------------------------------------------------------

-- | Normalize a relation by (partial) evaluation.
--
-- If the result is 'Left', then the relation could be fully evaluated and was
-- either a tautology ('Left True') or a contradiction ('Left False').
-- Otherwise, the result is a 'Right' value containing the maximally
-- evaluated/normalized relation. Note that not all tautological or
-- contradictory relations necessarily normalize.
normRel :: Rel -> Either Bool Rel
normRel = \case
-----------------------------------------------------------
  EUnit   _ :=: EUnit   _                     -> Left True
  EBool a _ :=: EBool b _                     -> Left (a == b)
  EInt  a _ :=: EInt  b _                     -> Left (a == b)
  EChar a _ :=: EChar b _                     -> Left (a == b)
  EStr  a _ :=: EStr  b _                     -> Left (a == b)
  a         :=: b         | a == b            -> Left True
  -----------------------------------------------------------
  EUnit   _ :≠: EUnit   _                     -> Left False
  EBool a _ :≠: EBool b _                     -> Left (a /= b)
  EInt  a _ :≠: EInt  b _                     -> Left (a /= b)
  EChar a _ :≠: EChar b _                     -> Left (a /= b)
  EStr  a _ :≠: EStr  b _                     -> Left (a /= b)
  a         :≠: b         | a == b            -> Left False
  -----------------------------------------------------------
  EInt a _ :<: EInt b _                       -> Left (a <  b)
  EInt a _ :≤: EInt b _                       -> Left (a <= b)
  EInt a _ :>: EInt b _                       -> Left (a >  b)
  EInt a _ :≥: EInt b _                       -> Left (a >= b)
  a        :<: b         | a == b             -> Left False
  a        :≤: b         | a == b             -> Left True
  a        :>: b         | a == b             -> Left False
  a        :≥: b         | a == b             -> Left True
  -----------------------------------------------------------
  -- NOTE: ">" is the structural ordering on 'Expr'; after 
  -- this block, the "smaller" expression will be on the LHS,
  -- with variables < functions < constants
  a :=: b | a > b                             -> normRel $ b :=: a
  a :≠: b | a > b                             -> normRel $ b :≠: a
  a :<: b | a > b                             -> normRel $ b :>: a
  a :≤: b | a > b                             -> normRel $ b :≥: a
  a :>: b | a > b                             -> normRel $ b :<: a
  a :≥: b | a > b                             -> normRel $ b :≤: a
  -----------------------------------------------------------
  ENot a :=: ENot b                           -> normRel $ a :=: b
  ENot a :≠: ENot b                           -> normRel $ a :≠: b
  ENot a :=: b                                -> normRel $ a :≠: b
  ENot a :≠: b                                -> normRel $ a :=: b
  a      :=: ENot b                           -> normRel $ a :≠: b
  a      :≠: ENot b                           -> normRel $ a :=: b
  -----------------------------------------------------------
  a :=: (b :+: EInt c _) | a == b             -> Left (c == 0)
  a :≠: (b :+: EInt c _) | a == b             -> Left (c /= 0)
  a :<: (b :+: EInt c _) | a == b             -> Left (c >  0)
  a :≤: (b :+: EInt c _) | a == b             -> Left (c >= 0)
  a :>: (b :+: EInt c _) | a == b             -> Left (c <  0)
  a :≥: (b :+: EInt c _) | a == b             -> Left (c <= 0)
  (b :+: EInt c _) :=: a | a == b             -> Left (c == 0)
  (b :+: EInt c _) :≠: a | a == b             -> Left (c /= 0)
  (b :+: EInt c _) :>: a | a == b             -> Left (c >  0)  
  (b :+: EInt c _) :≥: a | a == b             -> Left (c >= 0)
  (b :+: EInt c _) :<: a | a == b             -> Left (c <  0)
  (b :+: EInt c _) :≤: a | a == b             -> Left (c <= 0)
  -----------------------------------------------------------
  a :=: (b :-: EInt  c _) | a == b            -> Left (c == 0)
  a :≠: (b :-: EInt  c _) | a == b            -> Left (c /= 0)
  a :<: (b :-: EInt  c _) | a == b            -> Left (c <  0)
  a :≤: (b :-: EInt  c _) | a == b            -> Left (c <= 0)
  a :>: (b :-: EInt  c _) | a == b            -> Left (c >  0)
  a :≥: (b :-: EInt  c _) | a == b            -> Left (c >= 0)
  (b :-: EInt  c _) :=: a | a == b            -> Left (c == 0)
  (b :-: EInt  c _) :≠: a | a == b            -> Left (c /= 0)
  (b :-: EInt  c _) :>: a | a == b            -> Left (c <  0)
  (b :-: EInt  c _) :≥: a | a == b            -> Left (c <= 0)
  (b :-: EInt  c _) :<: a | a == b            -> Left (c >  0)
  (b :-: EInt  c _) :≤: a | a == b            -> Left (c >= 0)
  -----------------------------------------------------------
  a :<: (b :+: EInt 1 _)                      -> normRel $ a :≤: b
  a :≤: (b :-: EInt 1 _)                      -> normRel $ a :<: b
  a :>: (b :-: EInt 1 _)                      -> normRel $ a :≥: b
  a :≥: (b :+: EInt 1 _)                      -> normRel $ a :>: b
  -----------------------------------------------------------
  Rel op (a :+: b) c           | (a ⏚), (c ⏚) -> normRel $ Rel op b (normExpr $ c :-: a)
  Rel op (a :+: b) c           | (b ⏚), (c ⏚) -> normRel $ Rel op a (normExpr $ c :-: b)
  Rel op (a :+: b) c@(_ :+: d) | (a ⏚), (d ⏚) -> normRel $ Rel op b (normExpr $ c :-: a)
  Rel op (a :+: b) c@(_ :+: d) | (b ⏚), (d ⏚) -> normRel $ Rel op a (normExpr $ c :-: b)
  Rel op (a :+: b) c@(d :+: _) | (a ⏚), (d ⏚) -> normRel $ Rel op b (normExpr $ c :-: a)
  Rel op (a :+: b) c@(d :+: _) | (b ⏚), (d ⏚) -> normRel $ Rel op a (normExpr $ c :-: b)
  Rel op (a :+: b) c@(_ :-: d) | (a ⏚), (d ⏚) -> normRel $ Rel op b (normExpr $ c :-: a)
  Rel op (a :+: b) c@(_ :-: d) | (b ⏚), (d ⏚) -> normRel $ Rel op a (normExpr $ c :-: b)
  Rel op (a :+: b) c@(d :-: _) | (a ⏚), (d ⏚) -> normRel $ Rel op b (normExpr $ c :-: a)
  Rel op (a :+: b) c@(d :-: _) | (b ⏚), (d ⏚) -> normRel $ Rel op a (normExpr $ c :-: b)
  Rel op (a :-: b) c           | (a ⏚), (c ⏚) -> normRel $ Rel op b (normExpr $ a :-: c)
  Rel op (a :-: b) c           | (b ⏚), (c ⏚) -> normRel $ Rel op a (normExpr $ c :+: b)
  Rel op (a :-: b) c@(_ :+: d) | (a ⏚), (d ⏚) -> normRel $ Rel op b (normExpr $ a :-: c)
  Rel op (a :-: b) c@(_ :+: d) | (b ⏚), (d ⏚) -> normRel $ Rel op a (normExpr $ c :+: b)
  Rel op (a :-: b) c@(d :+: _) | (a ⏚), (d ⏚) -> normRel $ Rel op b (normExpr $ a :-: c)
  Rel op (a :-: b) c@(d :+: _) | (b ⏚), (d ⏚) -> normRel $ Rel op a (normExpr $ c :+: b)
  Rel op (a :-: b) c@(_ :-: d) | (a ⏚), (d ⏚) -> normRel $ Rel op b (normExpr $ a :-: c)
  Rel op (a :-: b) c@(_ :-: d) | (b ⏚), (d ⏚) -> normRel $ Rel op a (normExpr $ c :+: b)
  Rel op (a :-: b) c@(d :-: _) | (a ⏚), (d ⏚) -> normRel $ Rel op b (normExpr $ a :-: c)
  Rel op (a :-: b) c@(d :-: _) | (b ⏚), (d ⏚) -> normRel $ Rel op a (normExpr $ c :+: b)
  -----------------------------------------------------------
  EStrComp a :=: EStrComp b                   -> normRel $ a :=: b
  EStrComp a :≠: EStrComp b                   -> normRel $ a :≠: b
  EStrComp a :=: b                            -> normRel $ a :≠: b
  EStrComp a :≠: b                            -> normRel $ a :=: b
  a          :≠: EStrComp b                   -> normRel $ a :=: b
  -----------------------------------------------------------
  k :=: EStrIndexOf s t i | k == i 
    -> normRel $ EStrSub s i (i :+: (EStrLen t :-: EInt 1 NoPV)) :=: t
  -----------------------------------------------------------
  EStrSub s i1 i2 :=: EStr t pv
    | i1 == i2, [c] <- Text.unpack t -> normRel $ EStrAt s i1 :=: EChar c pv
  -----------------------------------------------------------
  r | r' <- descendBi normExpr r, r' /= r     -> normRel r'
    | otherwise                               -> Right r
