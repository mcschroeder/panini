module Panini.Error where

import Data.Text (Text)
import Data.Text qualified as Text
import Panini.Abstract.AValue
import Panini.Pretty
import Panini.Parser qualified
import Panini.Provenance
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude
import Panini.Diagnostic
import Control.Exception

-------------------------------------------------------------------------------

data Error
  = AlreadyDefined Name
  | UnknownVar Name
  | InvalidSubtype Type Type
  | ExpectedFunType Term Type
  | ParseError Panini.Parser.Error
  | SolverError Text PV
  | Unsolvable Name Con
  | IOError IOException
  | AbstractionImpossible Name ARel
  | AbstractionToValueImpossible Name ARel AValue
  | ConcretizationImpossible Name Base AValue

instance HasProvenance Error where
  getPV = \case
    AlreadyDefined x               -> getPV x
    UnknownVar x                   -> getPV x
    InvalidSubtype t _             -> getPV t
    ExpectedFunType e _            -> getPV e
    ParseError (Panini.Parser.ParserError _ pv) -> pv
    SolverError _ pv               -> pv
    Unsolvable x _                 -> getPV x
    IOError _                    -> NoPV
    AbstractionImpossible x _r1    -> getPV x -- TODO: getPV r1
    AbstractionToValueImpossible x _r1 _ -> getPV x -- TODO: getPV r1
    ConcretizationImpossible x _ _ -> getPV x -- TODO: getPV a
  
  setPV pv = \case
    AlreadyDefined x               -> AlreadyDefined (setPV pv x)
    UnknownVar x                   -> UnknownVar (setPV pv x)
    InvalidSubtype t1 t2           -> InvalidSubtype (setPV pv t1) t2
    ExpectedFunType e t            -> ExpectedFunType (setPV pv e) t
    ParseError (Panini.Parser.ParserError e _) -> ParseError (Panini.Parser.ParserError e pv)
    SolverError e _                -> SolverError e pv
    Unsolvable x vc                -> Unsolvable (setPV pv x) vc
    IOError e                    -> IOError e
    AbstractionImpossible x r1     -> AbstractionImpossible (setPV pv x) r1     -- TODO: setPV r1
    AbstractionToValueImpossible x r1 e  -> AbstractionToValueImpossible (setPV pv x) r1 e     -- TODO: setPV r1
    ConcretizationImpossible x b a -> ConcretizationImpossible (setPV pv x) b a -- TODO: setPV a

instance Diagnostic Error where
  diagnosticMessage = prettyErrorMessage

prettyErrorMessage :: Error -> Doc
prettyErrorMessage = \case
  AlreadyDefined x     -> "multiple definitions for" <\> pretty x    
  UnknownVar x         -> "unknown variable" <\> pretty x        
  InvalidSubtype t1 t2 -> "invalid subtype:" <\> pretty t1 <+> "<:" <+> pretty t2
  ExpectedFunType _ t  -> "invalid function type:" <\> pretty t
  ParseError (Panini.Parser.ParserError e _) -> pretty e
  SolverError e _      -> "unexpected SMT solver output:" <\> pretty e
  Unsolvable x _       -> "cannot solve constraints of" <\> pretty x    
  IOError e          -> pretty $ Text.pack $ displayException e
  AbstractionImpossible x r -> 
    "abstraction impossible:" <\> "⟦" <> pretty r <> "⟧↑" <> pretty x
  AbstractionToValueImpossible x r e ->
    "abstraction to value impossible:" <\> 
    "⟦" <> pretty r <> "⟧↑" <> pretty x <+> "≐" <+> pretty e
  ConcretizationImpossible x b a ->
    "concretization impossible for" <+> pretty b <> ":" <\> 
    "⟦" <> pretty a <> "⟧↓" <> pretty x
