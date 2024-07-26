module Panini.Frontend.Python.Error where

import Language.Python.Common.ParseError
import Language.Python.Common.Token
import Panini.Frontend.Python.AST
import Panini.Frontend.Python.Provenance
import Panini.Frontend.Python.Pretty ()
import Panini.Provenance
import Panini.Pretty
import Prelude

data Error where
  ParserError                 :: ParseError                       -> Error
  UnsupportedStatement        :: HasProvenance a => (Statement a) -> Error
  UnsupportedExpression       :: HasProvenance a => (Expr a)      -> Error
  UnsupportedTypeHint         :: HasProvenance a => (Expr a)      -> Error
  UnsupportedDefaultParameter :: HasProvenance a => (Expr a)      -> Error
  MissingParameterTypeHint    :: HasProvenance a => (Parameter a) -> Error
  UnsupportedParameter        :: HasProvenance a => (Parameter a) -> Error
  UnsupportedAtomicExpression :: HasProvenance a => (Expr a)      -> Error
  UnsupportedOperator         :: HasProvenance a => (Op a)        -> Error
  OtherError                  :: String                           -> Error -- TODO: remove

instance Pretty Error where
  pretty = \case
    ParserError (UnexpectedToken t)   -> "unexpected token:" <\> pretty t
    ParserError (UnexpectedChar c _)  -> "unexpected character:" <+> pretty c
    ParserError (StrError str)        -> pretty str
    UnsupportedStatement stmt         -> "unsupported statement:" <\> pretty stmt
    UnsupportedExpression expr        -> "unsupported expression:" <\> pretty expr
    UnsupportedTypeHint expr          -> "unsupported type hint:" <\> pretty expr
    UnsupportedDefaultParameter expr  -> "unsupported default parameter value:" <\> pretty expr
    MissingParameterTypeHint param    -> "missing type hint for parameter" <\> pretty param
    UnsupportedParameter param        -> "unsupported parameter:" <\> pretty param
    UnsupportedAtomicExpression expr  -> "unsupported atomic expression:" <\> pretty expr
    UnsupportedOperator op            -> "unsupported operator:" <\> pretty op
    OtherError err                    -> pretty err

instance HasProvenance Error where
  getPV = \case
    ParserError (UnexpectedToken t)   -> pySpanToPV (token_span t)
    ParserError (UnexpectedChar _ l)  -> pyLocToPV l
    ParserError (StrError _)          -> NoPV
    UnsupportedStatement stmt         -> getPV stmt
    UnsupportedExpression expr        -> getPV expr
    UnsupportedTypeHint expr          -> getPV expr
    UnsupportedDefaultParameter expr  -> getPV expr
    MissingParameterTypeHint param    -> getPV param
    UnsupportedParameter param        -> getPV param
    UnsupportedAtomicExpression expr  -> getPV expr
    UnsupportedOperator op            -> getPV op
    OtherError _                      -> NoPV
  
  setPV _ = undefined -- TODO: remove

