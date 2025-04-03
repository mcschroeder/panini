module Panini.CLI.Error where

import Panini.Diagnostic
import Panini.Elab.Error qualified as Elab
import Panini.Frontend.Python.Error qualified as Py
import Panini.Parser qualified as Parser
import Panini.Pretty
import Panini.Provenance
import Prelude

data AppError where
  -- | An error from the Python frontend.
  PythonError :: Py.Error -> AppError
  -- | An error from the Panini elaborator.
  ElabError   :: Elab.ElabError -> AppError
  -- | An error from the Panini parser.
  ParseError  :: Parser.Error -> AppError
  -- | Some I/O error from the CLI app.
  AppIOError  :: IOError -> AppError  

instance Diagnostic AppError where
  diagnosticMessage = \case
    PythonError e -> pretty e
    ElabError   e -> diagnosticMessage e
    ParseError  e -> diagnosticMessage e
    AppIOError  e -> diagnosticMessage e

instance HasProvenance AppError where
  getPV = \case
    PythonError e -> getPV e
    ElabError e   -> getPV e
    ParseError e  -> getPV e
    AppIOError _  -> NoPV
