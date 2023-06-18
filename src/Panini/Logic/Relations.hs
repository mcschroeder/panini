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
  | In  -- ^ regular expression inclusion @∈@
  | Ni  -- ^ regular language non-inclusion @∉@
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

-- | Inverse of a relation, e.g., ≥ to <.
invRel :: Rop -> Rop
invRel = \case
  Eq -> Ne
  Ne -> Eq
  Ge -> Lt
  Le -> Gt
  Gt -> Le
  Lt -> Ge
  In -> Ni
  Ni -> In

-- | Converse of a relation, e.g., ≥ to ≤.
convRel :: Rop -> Rop
convRel = \case
  Eq -> Eq
  Ne -> Ne
  Ge -> Le
  Le -> Ge
  Gt -> Lt
  Lt -> Gt
  In -> undefined
  Ni -> undefined

evalRel :: Ord a => Rop -> (a -> a -> Bool)
evalRel = \case
  Eq -> (==)
  Ne -> (/=)
  Ge -> (>=)
  Le -> (<=)
  Gt -> (>)
  Lt -> (<)
  In -> undefined
  Ni -> undefined
