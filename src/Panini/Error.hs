module Panini.Error where

import Data.Text (Text)
import Panini.Provenance
import Panini.Syntax
import Prelude

data Error
  = AlreadyDefined Name
  | VarNotInScope Name
  | MissingType Name
  | InvalidSubtypeBase (Type,Base) (Type,Base)
  | InvalidSubtype Type Type
  | ExpectedFunType Term Type
  | CantSynth Term
  | ParserError PV Text
  deriving stock (Show, Read)

instance HasProvenance Error where
  getPV (AlreadyDefined x) = getPV x
  getPV (VarNotInScope x) = getPV x
  getPV (MissingType x) = getPV x
  getPV (InvalidSubtypeBase (t,_) _) = getPV t
  getPV (InvalidSubtype t _) = getPV t
  getPV (ExpectedFunType _e _) = NoPV --getPV e
  getPV (CantSynth _e) = NoPV --getPV e
  getPV (ParserError pv _) = pv

  setPV pv (AlreadyDefined x) = AlreadyDefined (setPV pv x)
  setPV pv (VarNotInScope x) = VarNotInScope (setPV pv x)
  setPV pv (MissingType x) = MissingType (setPV pv x)
  setPV pv (InvalidSubtypeBase (t1,b1) y) = InvalidSubtypeBase (setPV pv t1, b1) y
  setPV pv (InvalidSubtype t1 t2) = InvalidSubtype (setPV pv t1) t2
  setPV _ e@(ExpectedFunType _e _) = e -- TODO
  setPV _ e@(CantSynth _e) = e -- TODO
  setPV pv (ParserError _ e) = ParserError pv e


