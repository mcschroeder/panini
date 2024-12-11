module Panini.Pretty.Fixity where 

import Prelude

class HasFixity a where
  fixity :: a -> Fixity

data Fixity = Prefix | Infix Associativity Int
data Associativity = LeftAss | RightAss | NoAss

needsParensLeftOf :: (HasFixity a, HasFixity b) => a -> b -> Bool
needsParensLeftOf a o = case (fixity a, fixity o) of
  (Infix _ lhs, Infix LeftAss  op) -> lhs <  op
  (Infix _ lhs, Infix NoAss    op) -> lhs <= op
  (Infix _ lhs, Infix RightAss op) -> lhs <= op
  _                                -> False

needsParensRightOf :: (HasFixity a, HasFixity b) => a -> b -> Bool
needsParensRightOf a o = case (fixity o, fixity a) of
  (Infix LeftAss  op, Infix _ rhs) -> op >= rhs
  (Infix NoAss    op, Infix _ rhs) -> op >= rhs
  (Infix RightAss op, Infix _ rhs) -> op >  rhs
  _                                -> False

needsParensPrefixedBy :: (HasFixity a, HasFixity b) => a -> b -> Bool
needsParensPrefixedBy a o = case (fixity o, fixity a) of
  (Prefix, Prefix) -> False
  _                -> True

