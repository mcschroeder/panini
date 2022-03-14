module Panini.Error where

import Panini.Syntax
import Prelude

data Error
  = AlreadyDefined Name
  | VarNotInScope Name
  | MissingType Name
  | InvalidSubtypeBase (Type,Base) (Type,Base)
  | InvalidSubtype Type Type
  | ExpectedFunType Expr Type
  | CantSynth Expr
  | ParserError String
  deriving stock (Show, Read)

-- TODO: add source location information to errors
