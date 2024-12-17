{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}
module Panini.Frontend.Python.Provenance where

import Language.Python.Common.AST
import Language.Python.Common.ParseError
import Language.Python.Common.SrcLocation
import Language.Python.Common.Token
import Panini.Provenance
import Prelude
import Panini.Frontend.Python.Typing.TypeInfo

------------------------------------------------------------------------------

convertProvenance :: ModuleSpan -> Module PV
convertProvenance (Module stmts) = Module $ map (fmap pySpanToPV) stmts

------------------------------------------------------------------------------

pySpanToPV :: SrcSpan -> PV
pySpanToPV = \case
  SpanEmpty -> NoPV
  sp -> FromSource SrcLoc{..} Nothing
   where 
    file  = sp.span_filename
    begin = (startRow sp, startCol sp)
    end   = (endRow   sp, endCol   sp + 1)

pyLocToPV :: SrcLocation -> PV
pyLocToPV = \case
  NoLocation -> NoPV
  sl -> FromSource SrcLoc{..} Nothing
   where
    file  = sl.sloc_filename
    begin = (sloc_row sl, sloc_column sl)
    end   = begin

------------------------------------------------------------------------------

getParseErrorPV :: ParseError -> PV
getParseErrorPV = \case
  UnexpectedToken t  -> pySpanToPV (token_span t)
  UnexpectedChar _ l -> pyLocToPV l
  StrError _         -> NoPV

------------------------------------------------------------------------------

instance HasProvenance a => HasProvenance (Ident a)     where getPV = getPV . annot
instance HasProvenance a => HasProvenance (Statement a) where getPV = getPV . annot
instance HasProvenance a => HasProvenance (Expr a)      where getPV = getPV . annot
instance HasProvenance a => HasProvenance (Parameter a) where getPV = getPV . annot
instance HasProvenance a => HasProvenance (AssignOp a)  where getPV = getPV . annot
instance HasProvenance a => HasProvenance (Op a)        where getPV = getPV . annot
instance HasProvenance a => HasProvenance (TypeInfo, a) where getPV = getPV . snd

instance Annotated ((,) b) where 
  annot = snd
