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
  | ParserError PV String String  -- ^ provenance offendingLine errorMsg
  deriving stock (Show, Read)

instance HasProvenance Error where
  getPV (AlreadyDefined x) = getPV x
  getPV (VarNotInScope x) = getPV x
  getPV (MissingType x) = getPV x
  getPV (InvalidSubtypeBase (_t,_) _) = UnknownPV --getPV t
  getPV (InvalidSubtype _t _) = UnknownPV --getPV t
  getPV (ExpectedFunType _e _) = UnknownPV --getPV e
  getPV (CantSynth _e) = UnknownPV --getPV e
  getPV (ParserError pv _ _) = pv
