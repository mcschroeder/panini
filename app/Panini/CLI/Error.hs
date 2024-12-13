module Panini.CLI.Error where

import Panini.Diagnostic
import Panini.Error qualified
import Panini.Frontend.Python.Error qualified as Py
import Panini.Provenance
import Panini.Pretty
import Panini.Parser qualified
import Panini.Monad ()
import Prelude

data AppError
  = PythonError Py.Error
  | PaniniError Panini.Error.Error
  | AppIOError IOError
  | ParseError Panini.Parser.Error

instance HasProvenance AppError where
  getPV = \case
    PythonError e -> getPV e
    PaniniError e -> getPV e
    AppIOError e -> getPV e
    ParseError (Panini.Parser.ParserError _ pv) -> pv
  
  setPV pv = \case
    PythonError e -> PythonError (setPV pv e)
    PaniniError e -> PaniniError (setPV pv e)
    AppIOError e -> AppIOError (setPV pv e)
    ParseError (Panini.Parser.ParserError e _) -> ParseError (Panini.Parser.ParserError e pv)

instance Diagnostic AppError where
  diagnosticMessage = \case
    PythonError e -> pretty e
    PaniniError e -> diagnosticMessage e
    AppIOError e -> diagnosticMessage e
    ParseError (Panini.Parser.ParserError e _) -> pretty e
