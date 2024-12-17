module Panini.CLI.Common where

import Control.Monad.Extra
import Data.Text.IO qualified as Text
import Panini.CLI.Error
import Panini.CLI.Options
import Panini.Diagnostic
import Panini.Elab.Module
import Panini.Monad
import Panini.Parser (parseProgram)
import Panini.Pretty as PP
import Prelude
import System.FilePath
import System.IO
import Panini.Frontend.Python

-------------------------------------------------------------------------------

loadModule :: PanOptions -> ModuleOrigin -> Pan AppError Module
loadModule panOpts origin = do
  (path,src) <- case origin of
    File fp -> do
      src <- tryIO (Text.readFile fp) ?? AppIOError §§ "Read" <+> pretty fp
      return (fp,src)
    Stdin s -> return ("<stdin>", s)
    REPL s -> return ("<repl>", s)
  let ext = takeExtension path  
  let sourceType | panOpts.pythonInput = "python"
                 | ext == ".py"        = "python"
                 | otherwise           = "panini"
  prog <- case sourceType of
    "python" -> transpilePythonProgram src path
    _        -> parseProgram path src ? ParseError §§ "Parse source"
  return $ Module origin sourceType prog

maybeSavePanFile :: PanOptions -> Module -> Pan AppError ()
maybeSavePanFile panOpts module_
  | not panOpts.savePanFile = return ()
  | inputIsPan = logMessage $ "Warning:" <+> warnIgnore
  | otherwise = do
      let pan = mkPanFile module_.moduleOrigin
      let opt = RenderOptions Nothing True Nothing
      let src = renderDoc opt $ pretty module_.program
      logMessage $ "Writing transpiled source to" <+> pretty pan
      liftIO $ Text.writeFile pan src
 where
  inputIsPan = module_.sourceType == "panini"
  warnIgnore = "ignoring --save-pan option; input is already a .pan file"
  mkPanFile (File  f) = replaceExtension f ".pan"
  mkPanFile (Stdin _) = "stdin.pan"
  mkPanFile (REPL  _) = "repl.pan" 

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
