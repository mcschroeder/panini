{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}
module Panini.Frontend.Python.Pretty where

import Language.Python.Common.ParseError
import Language.Python.Common.Pretty qualified as Py
import Language.Python.Common.PrettyAST ()
import Language.Python.Common.PrettyParseError ()
import Language.Python.Common.PrettyToken ()
import Language.Python.Common.SrcLocation as Py
import Language.Python.Common.Token
import Panini.Frontend.Python.AST
import Panini.Pretty
import Prelude
import Text.PrettyPrint qualified as TPP

------------------------------------------------------------------------------

instance Pretty (Module annot) where
  pretty = pretty . TPP.render . Py.pretty

instance Pretty (Ident annot) where
  pretty = pretty . TPP.render . Py.pretty

instance Pretty (Parameter annot) where
  pretty = pretty . TPP.render . Py.pretty

instance Pretty (Statement annot) where
  pretty = pretty . TPP.render . Py.pretty

instance Pretty (Expr annot) where
  pretty = pretty . TPP.render . Py.pretty

instance Pretty (ExceptClause annot) where
  pretty = pretty . TPP.render . Py.pretty

instance Pretty (Op annot) where
  pretty = pretty . TPP.render . Py.pretty

instance Pretty ParseError where
  pretty = pretty . TPP.render . Py.pretty

instance Pretty Token where
  pretty = pretty . TPP.render . Py.pretty

------------------------------------------------------------------------------

keywordSpan :: StatementSpan -> SrcSpan
keywordSpan stmt = case stmt of
  Import{}      -> mk 5
  FromImport{}  -> mk 3
  While{}       -> mk 4
  For{}         -> mk 2
  AsyncFor{}    -> mk 4
  Fun{}         -> mk 2
  AsyncFun{}    -> mk 4
  Class{}       -> mk 4
  Conditional{} -> mk 1
  Decorated{..} -> getSpan $ head decorated_decorators
  Return{}      -> mk 5
  Try{}         -> mk 2
  Raise{}       -> mk 4
  With{}        -> mk 3
  AsyncWith{}   -> mk 4
  Delete{}      -> mk 2
  Global{}      -> mk 5
  NonLocal{}    -> mk 7
  Assert{}      -> mk 5
  _             -> getSpan stmt
 where
  mk n = 
    let begin = spanStartPointLoc (getSpan stmt)
        end   = incColumn n begin
    in mkSrcSpan begin end

spanStartPointLoc :: SrcSpan -> Py.SrcLocation
spanStartPointLoc sp = case spanStartPoint sp of
  SpanPoint{..} -> Sloc { sloc_filename = span_filename
                        , sloc_row = span_row
                        , sloc_column = span_column
                        }
  _ -> NoLocation
