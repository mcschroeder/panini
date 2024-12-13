module Panini.CLI.Common where

import Control.Monad.Extra
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Panini.CLI.Error
import Panini.CLI.Options
import Panini.Diagnostic
import Panini.Elab
import Panini.Modules
import Panini.Monad
import Panini.Pretty as PP
import Panini.Syntax
import Prelude
import System.FilePath
import System.IO

-------------------------------------------------------------------------------

-- TODO: here may not be the right location for this
loadModule :: Text -> FilePath -> Pan AppError (Module, Program)
loadModule src fp = do
  module_ <- liftIO $ getModule fp
  prog <- parseSource (moduleLocation module_) src ?? ElabError
  return (module_, prog)

data FileType = PaniniSource | PythonSource

determineFileType :: PanOptions -> FilePath -> FileType
determineFileType panOpts fp
  | panOpts.pythonInput || ext == ".py" = PythonSource
  | otherwise                           = PaniniSource
 where
  ext = takeExtension fp

maybeSavePanFile :: PanOptions -> Module -> Program -> Pan AppError ()
maybeSavePanFile panOpts module_ prog
  | not panOpts.savePanFile = return ()
  | inputIsPan = logMessage $ "Warning:" <+> warnIgnore      
  | otherwise = do
      let pan = mkPanFile
      let opt = RenderOptions Nothing True Nothing
      let src = renderDoc opt $ pretty prog
      logMessage $ "Writing transpiled source to" <+> pretty pan
      liftIO $ Text.writeFile pan src
 where
  inputIsPan = takeExtension (moduleLocation module_) == ".pan"
  warnIgnore = "ignoring --save-pan option; input is already a .pan file"
  mkPanFile | module_ == replModule = "repl.pan"
            | module_ == stdinModule = "stdin.pan"
            | otherwise = replaceExtension (moduleLocation module_) ".pan"

-------------------------------------------------------------------------------

openLogFileFor :: FilePath -> IO Handle
openLogFileFor f = do
  h <- openFile (f -<.> "log") WriteMode
  hSetBuffering h NoBuffering
  return h

putDiagnosticFile :: Diagnostic a => PanOptions -> DiagnosticEnvelope a -> Handle -> IO ()
putDiagnosticFile o e = putDocFile o $ prettyDiagnostic e <> "\n"

putDiagnosticStderr :: Diagnostic a => PanOptions -> DiagnosticEnvelope a -> IO ()
putDiagnosticStderr o e = putDocStderr o $ prettyDiagnostic e <> "\n"

prettyDiagnostic :: Diagnostic a => DiagnosticEnvelope a -> Doc
prettyDiagnostic diagEnv = case diagEnv.severity of
  SevError -> prettyErrorDiagnostic msg diagEnv.provenance
  SevWarning -> ann Error "warning:" <+> align msg
  SevInfo -> ann Margin (pretty src) <+> align msg
  SevTrace ->
    ann Margin (divider symDivH2 (Just $ Left src)) <\\> 
    msg <\\>
    ann Margin (divider symDivH2 Nothing)
 where
  src = "(" ++ diagEnv.rapporteur ++ ")"
  msg = diagnosticMessage diagEnv.diagnostic

-------------------------------------------------------------------------------

putDocFile :: PanOptions -> Doc -> Handle -> IO ()
putDocFile o d h = hPutDoc (fileRenderOptions o) d h

putDocStderr :: PanOptions -> Doc -> IO ()
putDocStderr o d = hPutDoc (termRenderOptions o) d stderr

putDocStdout :: PanOptions -> Doc -> IO ()
putDocStdout o d = hPutDoc (termRenderOptions o) d stdout

hPutDoc :: RenderOptions -> Doc -> Handle -> IO ()
hPutDoc o d h = Text.hPutStr h $ renderDoc o d

-------------------------------------------------------------------------------

fileRenderOptions :: PanOptions -> RenderOptions
fileRenderOptions o = RenderOptions 
  { styling = Nothing
  , PP.unicode = o.unicode
  , fixedWidth = Nothing 
  }

termRenderOptions :: PanOptions -> RenderOptions
termRenderOptions o = RenderOptions
  { styling = pureIf o.color defaultStyling
  , PP.unicode = o.unicode
  , fixedWidth = o.termWidth
  }
