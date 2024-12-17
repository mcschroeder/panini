module Panini.Elab.Error where

import Panini.Diagnostic
import Panini.Parser qualified as Parser
import Panini.Pretty
import Panini.Provenance
import Panini.Solver.Constraints
import Panini.Solver.Error qualified as Solver
import Panini.Syntax
import Prelude

------------------------------------------------------------------------------

data ElabError where
  AlreadyDefined  :: Name -> ElabError
  Unsolvable      :: Name -> Con -> ElabError
  SolverError     :: Solver.Error -> ElabError
  TypeError       :: TypeError -> ElabError
  ParseError      :: Parser.Error -> ElabError
  IOError         :: IOError -> ElabError

instance Diagnostic ElabError where
  diagnosticMessage = \case
    AlreadyDefined x -> "multiple definitions for" <\> pretty x
    Unsolvable x _ -> "cannot solve constraints of" <\> pretty x
    TypeError e -> diagnosticMessage e
    SolverError e -> diagnosticMessage e
    ParseError e -> diagnosticMessage e
    IOError e -> diagnosticMessage e

instance HasProvenance ElabError where
  getPV = \case
    AlreadyDefined x -> getPV x
    Unsolvable x _ -> getPV x
    TypeError e -> getPV e
    SolverError e -> getPV e
    ParseError e -> getPV e
    IOError _ -> NoPV

------------------------------------------------------------------------------

data TypeError where
  UnknownVar        :: Name -> TypeError
  InvalidSubtype    :: Type -> Type -> TypeError
  ExpectedFunType   :: Term -> Type -> TypeError

instance Diagnostic TypeError where
  diagnosticMessage = \case
    UnknownVar x         -> "unknown variable" <\> pretty x
    InvalidSubtype t1 t2 -> "invalid subtype:" <\> pretty t1 <+> "<:" <+> pretty t2
    ExpectedFunType _ t  -> "invalid function type:" <\> pretty t
 
instance HasProvenance TypeError where
  getPV = \case
    UnknownVar x        -> getPV x
    InvalidSubtype t _  -> getPV t
    ExpectedFunType e _ -> getPV e
