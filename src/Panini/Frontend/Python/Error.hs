module Panini.Frontend.Python.Error where

import Language.Python.Common.ParseError
import Language.Python.Common.Token
import Panini.Frontend.Python.AST
import Panini.Frontend.Python.Provenance
import Panini.Frontend.Python.Pretty ()
import Panini.Provenance
import Panini.Pretty
import Prelude

data Error
  = ParserError ParseError
  | UnsupportedStatement StatementSpan
  | UnsupportedExpression ExprSpan    
  | UnsupportedTypeHint ExprSpan
  | UnsupportedDefaultParameter ExprSpan
  | MissingParameterTypeHint ParameterSpan
  | UnsupportedParameter ParameterSpan
  | UnsupportedAtomicExpression ExprSpan
  | UnsupportedOperator OpSpan  
  | OtherError String  -- TODO: remove
  deriving stock (Show)

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

-- TODO: split HasProvenance into separate getter and setter classes
getPV' :: Error -> PV
getPV' = \case
  ParserError (UnexpectedToken t)   -> pySpanToPV (token_span t)
  ParserError (UnexpectedChar _ l)  -> pyLocToPV l
  ParserError (StrError _)          -> NoPV
  UnsupportedStatement stmt         -> pySpanToPV (annot stmt)
  UnsupportedExpression expr        -> pySpanToPV (annot expr)
  UnsupportedTypeHint expr          -> pySpanToPV (annot expr)
  UnsupportedDefaultParameter expr  -> pySpanToPV (annot expr)
  MissingParameterTypeHint param    -> pySpanToPV (annot param)
  UnsupportedParameter param        -> pySpanToPV (annot param)
  UnsupportedAtomicExpression expr  -> pySpanToPV (annot expr)
  UnsupportedOperator op            -> pySpanToPV (annot op)
  OtherError _                      -> NoPV

