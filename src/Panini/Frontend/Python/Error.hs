module Panini.Frontend.Python.Error where

import Language.Python.Common.ParseError
import Panini.Frontend.Python.AST
import Panini.Frontend.Python.Pretty ()
import Panini.Frontend.Python.Typing.Monad (TypeError)
import Panini.Pretty
import Panini.Provenance
import Prelude

data Error where
  ParserError                 :: ParseError -> PV                 -> Error
  TypeError                   :: TypeError                        -> Error
  UnsupportedStatement        :: HasProvenance a => (Statement a) -> Error
  UnsupportedExpression       :: HasProvenance a => (Expr a)      -> Error
  UnsupportedTypeHint         :: HasProvenance a => (Expr a)      -> Error
  UnsupportedDefaultParameter :: HasProvenance a => (Expr a)      -> Error
  UnsupportedParameter        :: HasProvenance a => (Parameter a) -> Error
  UnsupportedAtomicExpression :: HasProvenance a => (Expr a)      -> Error
  UnsupportedOperator         :: HasProvenance a => (Op a)        -> Error
  OtherError                  :: String -> PV                     -> Error -- TODO: remove

instance Pretty Error where
  pretty = \case
    ParserError (UnexpectedToken t)  _ -> "unexpected token:" <\> pretty t
    ParserError (UnexpectedChar c _) _ -> "unexpected character:" <+> pretty c
    ParserError (StrError str)       _ -> pretty str
    TypeError err                     -> "type error:" <+> pretty err
    UnsupportedStatement stmt         -> "unsupported statement:" <\> pretty stmt
    UnsupportedExpression expr        -> "unsupported expression:" <\> pretty expr
    UnsupportedTypeHint expr          -> "unsupported type hint:" <\> pretty expr
    UnsupportedDefaultParameter expr  -> "unsupported default parameter value:" <\> pretty expr
    UnsupportedParameter param        -> "unsupported parameter:" <\> pretty param
    UnsupportedAtomicExpression expr  -> "unsupported atomic expression:" <\> pretty expr
    UnsupportedOperator op            -> "unsupported operator:" <\> pretty op
    OtherError err _                  -> pretty err

instance HasProvenance Error where
  getPV = \case
    ParserError _ pv                  -> pv
    TypeError _                       -> NoPV -- TODO
    UnsupportedStatement stmt         -> getPV $ annot stmt
    UnsupportedExpression expr        -> getPV $ annot expr
    UnsupportedTypeHint expr          -> getPV $ annot expr
    UnsupportedDefaultParameter expr  -> getPV $ annot expr
    UnsupportedParameter param        -> getPV $ annot param
    UnsupportedAtomicExpression expr  -> getPV $ annot expr
    UnsupportedOperator op            -> getPV $ annot op
    OtherError _ pv                   -> pv
  
  setPV pv = \case
    ParserError e                   _ -> ParserError e pv
    TypeError _                       -> undefined -- TODO
    UnsupportedStatement stmt         -> UnsupportedStatement        $ fmap (setPV pv) stmt
    UnsupportedExpression expr        -> UnsupportedExpression       $ fmap (setPV pv) expr
    UnsupportedTypeHint expr          -> UnsupportedTypeHint         $ fmap (setPV pv) expr
    UnsupportedDefaultParameter expr  -> UnsupportedDefaultParameter $ fmap (setPV pv) expr
    UnsupportedParameter param        -> UnsupportedParameter        $ fmap (setPV pv) param
    UnsupportedAtomicExpression expr  -> UnsupportedAtomicExpression $ fmap (setPV pv) expr
    UnsupportedOperator op            -> UnsupportedOperator         $ fmap (setPV pv) op
    OtherError e _                    -> OtherError e pv
