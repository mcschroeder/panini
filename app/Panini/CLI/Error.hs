module Panini.CLI.Error where

import Panini.Diagnostic
import Panini.Elab.Error
import Panini.Frontend.Python.Error qualified as Py
import Panini.Provenance
import Panini.Pretty
import Prelude

data AppError where
  -- | An error from the Python frontend.
  PythonError :: Py.Error -> AppError  
  -- | An error from the Panini elaborator.
  ElabError   :: ElabError -> AppError    
  -- | Some I/O error from the CLI app.
  AppIOError  :: IOError -> AppError

instance Diagnostic AppError where
  diagnosticMessage = \case
    PythonError e -> pretty e
    ElabError   e -> diagnosticMessage e
    AppIOError  e -> diagnosticMessage e

instance HasProvenance AppError where
  getPV = \case
    PythonError e -> getPV e
    ElabError e   -> getPV e
    AppIOError _  -> NoPV
