module Panini.Events where

import Panini.Error
import Panini.Pretty
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
    ann Error (divider symDivH (Just $ Right "ERROR")) <\\> pretty err <> "\n"
  LogMessage src msg -> ann Margin ("(" <> pretty src <> ")") <+> align msg
  LogData src dat -> 
    ann Margin (divider symDivH2 (Just $ Left $ "(" <> src <> ")")) <\\> 
    dat <\\>
    ann Margin (divider symDivH2 Nothing)
  SMTSolverInitialized{_version} -> ann Margin $ "(SMT)" <+> pretty _version

isErrorEvent :: Event -> Bool
isErrorEvent (ErrorEvent _) = True
isErrorEvent _              = False
