module Panini.Logic.Relations where

import Data.Generics.Uniplate.Direct
import Data.Hashable
import GHC.Generics (Generic)
import Panini.Logic.Expressions
import Panini.Pretty.Printer
import Prelude

------------------------------------------------------------------------------

-- | Relation between expressions.
data Rel = Rel Rop PExpr PExpr    -- ^ binary relation @e₁ ⋈ e₂@  
  deriving stock (Eq, Show, Read, Generic)

-- | Returns the left-hand side of a relation.
leftSide :: Rel -> PExpr
leftSide (Rel _ e1 _) = e1

-- | Returns the right-hand side of a relation.
rightSide :: Rel -> PExpr
rightSide (Rel _ _ e2) = e2

-- TODO: replace with Complementable instance
-- | The inverse of a relation, e.g., @a > b@ to @a ≤ b@.
-- Note that this changes the semantics of the relation!
inverse :: Rel -> Rel
inverse = \case
  e1 :=: e2 -> e1 :≠: e2
  e1 :≠: e2 -> e1 :=: e2
  e1 :≥: e2 -> e1 :<: e2
  e1 :≤: e2 -> e1 :>: e2
  e1 :>: e2 -> e1 :≤: e2
  e1 :<: e2 -> e1 :>: e2
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

{-# COMPLETE (:=:), (:≠:), (:≥:), (:>:), (:≤:), (:<:), (:∈:), (:∉:) #-}

pattern (:=:), (:≠:) :: PExpr -> PExpr -> Rel
pattern e1 :=: e2 = Rel Eq e1 e2
pattern e1 :≠: e2 = Rel Ne e1 e2

pattern  (:≥:), (:>:), (:≤:), (:<:) :: PExpr -> PExpr -> Rel
pattern e1 :≥: e2 = Rel Ge e1 e2
pattern e1 :>: e2 = Rel Gt e1 e2
pattern e1 :≤: e2 = Rel Le e1 e2
pattern e1 :<: e2 = Rel Lt e1 e2

pattern (:∈:), (:∉:) :: PExpr -> PExpr -> Rel
pattern e1 :∈: e2 = Rel In e1 e2
pattern e1 :∉: e2 = Rel Ni e1 e2

instance Hashable Rel

instance Biplate Rel PExpr where
  biplate (Rel r e1 e2) = plate Rel |- r |* e1 |* e2

instance Pretty Rel where
  pretty p0 = case p0 of
    Rel r p1 p2 -> prettyL p0 p1 <+> pretty r   <+> prettyR p0 p2

instance HasFixity Rel where
  fixity _ = Infix NoAss 4

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
    In -> "∈" -- TODO
    Ni -> "∉" -- TODO
