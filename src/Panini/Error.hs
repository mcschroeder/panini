module Panini.Error where

import Data.Text (Text)
import Panini.Provenance
import Panini.Syntax
import Prelude

data Error
  = AlreadyDefined Name PV
  | VarNotInScope Name
  | MissingType Name
  | InvalidSubtypeBase (Type,Base) (Type,Base)
  | InvalidSubtype Type Type
  | ExpectedFunType Term Type
  | CantSynth Term
  | ParserError PV Text
  deriving stock (Show, Read)

instance HasProvenance Error where
  getPV (AlreadyDefined _ pv) = pv
  getPV (VarNotInScope _) = NoPV
  getPV (MissingType _) = NoPV
  getPV (InvalidSubtypeBase (_t,_) _) = NoPV --getPV t
  getPV (InvalidSubtype _t _) = NoPV --getPV t
  getPV (ExpectedFunType _e _) = NoPV --getPV e
  getPV (CantSynth _e) = NoPV --getPV e
  getPV (ParserError pv _) = pv

  setPV pv (AlreadyDefined x _) = AlreadyDefined x pv
  setPV _ e@(VarNotInScope _) = e
  setPV _ e@(MissingType _) = e
  setPV _ e@(InvalidSubtypeBase (_t,_) _) = e -- TODO
  setPV _ e@(InvalidSubtype _t _) = e -- TODO
  setPV _ e@(ExpectedFunType _e _) = e -- TODO
  setPV _ e@(CantSynth _e) = e -- TODO
  setPV pv (ParserError _ e) = ParserError pv e


