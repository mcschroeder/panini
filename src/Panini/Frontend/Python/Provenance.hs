module Panini.Frontend.Python.Provenance where

import Language.Python.Common.SrcLocation
import Panini.Provenance
import Prelude

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