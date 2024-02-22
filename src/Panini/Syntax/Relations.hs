module Panini.Syntax.Relations where

import Data.Generics.Uniplate.Direct
import Data.Generics.Uniplate.Operations as Uniplate
import Data.Hashable
import GHC.Generics (Generic)
import Panini.Pretty
import Panini.Syntax.Expressions
import Panini.Syntax.Substitution
import Prelude

------------------------------------------------------------------------------

-- | Relation between expressions.
data Rel = Rel !Rop !Expr !Expr    -- ^ binary relation @e₁ ⋈ e₂@  
  deriving stock (Eq, Show, Read, Generic)

-- | Returns the left-hand side of a relation.
leftSide :: Rel -> Expr
leftSide (Rel _ e1 _) = e1

-- | Returns the right-hand side of a relation.
rightSide :: Rel -> Expr
rightSide (Rel _ _ e2) = e2

-- | The inverse of a relation, e.g., @a > b@ to @a ≤ b@.
-- Note that this changes the semantics of the relation!
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

-- | The converse of a relation, e.g., @a > b@ to @b < a@, if it exists. This
-- transformation switches the left and right sides of the relation, while
-- keeping the truth value the same.
converse :: Rel -> Maybe Rel
converse = \case
  e1 :=: e2 -> Just $ e2 :=: e1
  e1 :≠: e2 -> Just $ e2 :≠: e1
  e1 :≥: e2 -> Just $ e2 :≤: e1
  e1 :≤: e2 -> Just $ e2 :≥: e1
  e1 :>: e2 -> Just $ e2 :<: e1
  e1 :<: e2 -> Just $ e2 :>: e1
  _ :∈: _ -> Nothing
  _ :∉: _ -> Nothing

-- | Whether a relation is an obvious tautology. Note that if this returns
-- 'False', the relation might still be a tautology (just not an obvious one).
isTaut :: Rel -> Bool
isTaut = \case
  e1 :=: e2 -> e1 == e2
  e1 :≤: e2 -> e1 == e2
  e1 :≥: e2 -> e1 == e2
  _         -> False

-- | Whether a relation is an obvious contradiction; see 'isTaut'.
isCont :: Rel -> Bool
isCont = \case
  e1 :≠: e2 -> e1 == e2
  e1 :<: e2 -> e1 == e2
  e1 :>: e2 -> e1 == e2
  _         -> False

{-# COMPLETE (:=:), (:≠:), (:≥:), (:>:), (:≤:), (:<:), (:∈:), (:∉:) #-}

pattern (:=:), (:≠:) :: Expr -> Expr -> Rel
pattern e1 :=: e2 = Rel Eq e1 e2
pattern e1 :≠: e2 = Rel Ne e1 e2

pattern  (:≥:), (:>:), (:≤:), (:<:) :: Expr -> Expr -> Rel
pattern e1 :≥: e2 = Rel Ge e1 e2
pattern e1 :>: e2 = Rel Gt e1 e2
pattern e1 :≤: e2 = Rel Le e1 e2
pattern e1 :<: e2 = Rel Lt e1 e2

pattern (:∈:), (:∉:) :: Expr -> Expr -> Rel
pattern e1 :∈: e2 = Rel In e1 e2
pattern e1 :∉: e2 = Rel Ni e1 e2

-- | Matches any relation, discarding the operator.
pattern (:⋈:) :: Expr -> Expr -> Rel
pattern e1 :⋈: e2 <- Rel _ e1 e2

instance Hashable Rel

instance Uniplate Rel where
  uniplate = plate

instance Biplate Rel Expr where
  biplate (Rel r e1 e2) = plate Rel |- r |* e1 |* e2

instance Pretty Rel where
  pretty p0 = case p0 of
    Rel r p1 p2 -> prettyL p0 p1 <+> pretty r <+> prettyR p0 p2

instance HasFixity Rel where
  fixity _ = Infix NoAss 4

instance Subable Rel Expr where
  subst x y = descendBi (subst @Expr x y)  
  freeVars = mconcat . map (freeVars @Expr) . universeBi

-- | A relation between two expressions.
data Rop 
  = Eq  -- ^ equal @=@
  | Ne  -- ^ unequal @≠@
  | Ge  -- ^ greater than or equal @≥@
  | Le  -- ^ less than or equal @≤@
  | Gt  -- ^ greater than @>@
  | Lt  -- ^ less than @<@
  | In  -- ^ included in @∈@
  | Ni  -- ^ not included in @∉@
  deriving stock (Eq, Ord, Generic, Show, Read)

instance Hashable Rop

instance Pretty Rop where
  pretty = \case
    Ne -> symNe
    Eq -> symEq
    Le -> symLe
    Lt -> symLt
    Ge -> symGe
    Gt -> symGt
    In -> symIn
    Ni -> symNi

------------------------------------------------------------------------------

-- | Normalize a relation and its expressions into a more common/simpler form.
-- Note that this operation does not introduce any abstract values.
normRel :: Rel -> Rel
normRel = Uniplate.rewrite $ \case
  
  ENot e1 :=: ENot e2 -> Just $ e1 :=: e2
  ENot e1 :=: e2      -> Just $ e1 :≠: e2
  e1      :=: ENot e2 -> Just $ e1 :≠: e2
  ENot e1 :≠: ENot e2 -> Just $ e1 :≠: e2
  ENot e1 :≠: e2      -> Just $ e1 :=: e2
  e1      :≠: ENot e2 -> Just $ e1 :=: e2

  IntComp e1 :=: IntComp e2 -> Just $ e1 :=: e2
  IntComp e1 :=: e2         -> Just $ e1 :≠: e2
  e1         :=: IntComp e2 -> Just $ e1 :≠: e2
  IntComp e1 :≠: IntComp e2 -> Just $ e1 :≠: e2

  StrComp e1 :=: StrComp e2 -> Just $ e1 :=: e2
  StrComp e1 :=: e2         -> Just $ e1 :≠: e2
  e1         :=: StrComp e2 -> Just $ e1 :≠: e2
  StrComp e1 :≠: StrComp e2 -> Just $ e1 :≠: e2    

  (StrAt_index s c      ) :=: i -> Just $ EStrAt s (i      ) :=: c
  (StrAt_index s c :+: y) :=: i -> Just $ EStrAt s (i :-: y) :=: c
  (StrAt_index s c :-: y) :=: i -> Just $ EStrAt s (i :+: y) :=: c
  i :=: (StrAt_index s c      ) -> Just $ EStrAt s (i      ) :=: c
  i :=: (StrAt_index s c :+: y) -> Just $ EStrAt s (i :-: y) :=: c
  i :=: (StrAt_index s c :-: y) -> Just $ EStrAt s (i :+: y) :=: c

  (StrAt_index s c      ) :≠: i -> Just $ EStrAt s (i      ) :≠: c
  (StrAt_index s c :+: y) :≠: i -> Just $ EStrAt s (i :-: y) :≠: c
  (StrAt_index s c :-: y) :≠: i -> Just $ EStrAt s (i :+: y) :≠: c
  i :≠: (StrAt_index s c      ) -> Just $ EStrAt s (i      ) :≠: c
  i :≠: (StrAt_index s c :+: y) -> Just $ EStrAt s (i :-: y) :≠: c
  i :≠: (StrAt_index s c :-: y) -> Just $ EStrAt s (i :+: y) :≠: c

  Rel op l1 r1 -> case (normExpr l1, normExpr r1) of 
    (l2,r2) | l1 /= l2 || r1 /= r2 -> Just $ Rel op l2 r2
            | otherwise            -> Nothing
