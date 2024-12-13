module Panini.Events where

import Panini.Diagnostic
import Panini.Pretty
import Panini.Provenance
import Prelude

-------------------------------------------------------------------------------

data Event 
  = forall d. Diagnostic d => ErrorEvent d
  | LogMessage String Doc
  | LogData String Doc
  | SMTSolverInitialized { _version :: String }

instance HasProvenance Event where
  getPV = \case
    ErrorEvent e -> getPV e
    _ -> NoPV
  
  setPV pv = \case
    ErrorEvent e -> ErrorEvent (setPV pv e)
    e -> e

-- TODO: move this kind of formatting out of here
prettyEvent :: Event -> Doc
prettyEvent = \case
  ErrorEvent err -> 
    ann Error (divider symDivH (Just $ Right "ERROR")) <\\> prettyDiagnostic err <> "\n"
  LogMessage src msg -> ann Margin ("(" <> pretty src <> ")") <+> align msg
  LogData src dat -> 
    ann Margin (divider symDivH2 (Just $ Left $ "(" <> src <> ")")) <\\> 
    dat <\\>
    ann Margin (divider symDivH2 Nothing)
  SMTSolverInitialized{_version} -> ann Margin $ "(SMT)" <+> pretty _version

isErrorEvent :: Event -> Bool
isErrorEvent (ErrorEvent _) = True
isErrorEvent _              = False
