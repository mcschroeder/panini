{-# OPTIONS_GHC -Wno-orphans #-}
module Panini.Frontend.Python.Pretty where

import Language.Python.Common.AST
import Language.Python.Common.Pretty qualified as Py
import Language.Python.Common.PrettyAST ()
import Panini.Pretty
import Prelude
import Text.PrettyPrint qualified as TPP

instance Pretty IdentSpan where
  pretty = pretty . TPP.render . Py.pretty

instance Pretty ParameterSpan where
  pretty = pretty . TPP.render . Py.pretty

instance Pretty StatementSpan where
  pretty = pretty . TPP.render . Py.pretty

instance Pretty ExprSpan where
  pretty = pretty . TPP.render . Py.pretty

instance Pretty ExceptClauseSpan where
  pretty = pretty . TPP.render . Py.pretty
