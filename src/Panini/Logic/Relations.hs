module Panini.Logic.Relations where

import Data.Generics.Uniplate.Direct
import Data.Hashable
import GHC.Generics (Generic)
import Panini.Logic.Expressions
import Panini.Pretty.Printer
import Panini.Primitives
import Prelude

------------------------------------------------------------------------------

-- | Relation between expressions.
data Rel
  = Rel Rop PExpr PExpr    -- ^ binary relation @e₁ ⋈ e₂@
  
  -- TODO: replace with proper RE type
  | PReg Value String       -- ^ regular language membership @v ∈ RE@
  deriving stock (Eq, Show, Read, Generic)

instance Hashable Rel

instance Biplate Rel PExpr where
  biplate = \case
    Rel r e1 e2  -> plate Rel |- r |* e1 |* e2
    PReg v re -> plate PReg |- v |- re

instance Pretty Rel where
  pretty p0 = case p0 of
    Rel r p1 p2 -> prettyL p0 p1 <+> pretty r   <+> prettyR p0 p2
    PReg v re -> pretty v <+> "∈" <+> pretty re  -- TODO: symQuery/symIn

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

-- | Inverse of a relation, e.g., ≥ to <.
invRel :: Rop -> Rop
invRel = \case
  Eq -> Ne
  Ne -> Eq
  Ge -> Lt
  Le -> Gt
  Gt -> Le
  Lt -> Ge

-- | Converse of a relation, e.g., ≥ to ≤.
convRel :: Rop -> Rop
convRel = \case
  Eq -> Eq
  Ne -> Ne
  Ge -> Le
  Le -> Ge
  Gt -> Lt
  Lt -> Gt

evalRel :: Ord a => Rop -> (a -> a -> Bool)
evalRel = \case
  Eq -> (==)
  Ne -> (/=)
  Ge -> (>=)
  Le -> (<=)
  Gt -> (>)
  Lt -> (<)
