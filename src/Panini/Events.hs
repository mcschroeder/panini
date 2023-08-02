module Panini.Events where

import Panini.Error
import Panini.Pretty.Printer
import Prelude

-------------------------------------------------------------------------------

data Event 
  = ErrorEvent Error
  | LogMessage String Doc
  | LogData String Doc

-- TODO: move this kind of formatting out of here
prettyEvent :: Event -> Doc
prettyEvent = \case
  ErrorEvent err -> anError (divider "ERROR") <\\> pretty err <> "\n"
  LogMessage src msg -> marginalia (pretty src <+> symDivDiag) <+> aMessage msg
  LogData label dat -> marginalia (divider label) <\\> dat <> "\n"
