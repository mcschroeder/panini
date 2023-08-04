module Panini.Events where

import Panini.Error
import Panini.Pretty.Printer
import Prelude

-------------------------------------------------------------------------------

data Event 
  = ErrorEvent Error
  | LogMessage String Doc
  | LogData String Doc
  | SMTSolverInitialized { _version :: String }

-- TODO: move this kind of formatting out of here
prettyEvent :: Event -> Doc
prettyEvent = \case
  ErrorEvent err -> 
    anError (divider symDivH (Just $ Right "ERROR")) <\\> pretty err <> "\n"
  LogMessage src msg -> marginalia ("(" <> pretty src <> ")") <+> align msg
  LogData src dat -> 
    marginalia (divider symDivH2 (Just $ Left $ "(" <> src <> ")")) <\\> 
    dat <\\>
    marginalia (divider symDivH2 Nothing) <> "\n"
  SMTSolverInitialized{_version} -> marginalia $ "(SMT)" <+> pretty _version

isErrorEvent :: Event -> Bool
isErrorEvent (ErrorEvent _) = True
isErrorEvent _              = False
