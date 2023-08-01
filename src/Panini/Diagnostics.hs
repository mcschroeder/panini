module Panini.Diagnostics where

import Panini.Error
import Panini.Pretty.Printer
import Prelude

data Diagnostic 
  = DiagError Error  -- TODO: rename
  | Warning Doc
  | DiagMessage String Doc  -- TODO: rename
  | Data String Doc

-- TODO: move this kind of formatting out of here
prettyDiagnostic :: Diagnostic -> Doc
prettyDiagnostic = \case
  DiagError err -> anError (divider "ERROR") <\\> pretty err <> "\n"
  Warning w -> w
  DiagMessage src msg -> marginalia (pretty src <+> symDivDiag) <+> aMessage msg
  Data label dat -> marginalia (divider label) <\\> dat <> "\n"
