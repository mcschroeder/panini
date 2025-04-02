module Panini.CLI.Common where

import Control.Monad.Extra
import Data.Maybe
import Data.Text.IO qualified as Text
import Panini.CLI.Error
import Panini.CLI.Options
import Panini.Diagnostic
import Panini.Elab.Module
import Panini.Monad
import Panini.Parser (parseProgram)
import Panini.Pretty as PP
import Prelude
import System.Console.ANSI
import System.FilePath
import System.IO
import Text.Printf
import Panini.Frontend.Python
import Panini.Provenance

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

withDiagnosticLogger :: PanOptions -> (DiagnosticHandler -> IO a) -> IO a
withDiagnosticLogger panOpts m
  | not (panOpts.trace || panOpts.traceToFile) = m (\_ -> pure ())
  | otherwise = do
      -- TODO: get trace file as CLI option
      traceFile <- whenMaybe panOpts.traceToFile
                    (openLogFileFor $ fromMaybe "stdin" panOpts.inputFile)            
      
      putFile <- case traceFile of
        Nothing -> return $ \_ -> pure ()
        Just h -> getPutDoc panOpts h
      
      putTerm <- getPutDoc panOpts stderr

      when panOpts.traceToFile $ putFile (logSourceCeiling <> "\n")
      when panOpts.trace       $ putTerm (logSourceCeiling <> "\n")

      x <- m $ \ev0 -> do
        pv' <- addSourceLines ev0.provenance
        let ev = ev0 { provenance = pv' }
        let doc = prettyDiagnostic ev <> "\n"
        when panOpts.traceToFile $ putFile doc
        when (panOpts.trace || (isError ev && not panOpts.testMode)) $ putTerm doc
      
      when panOpts.traceToFile $ putFile (logSourceFloor <> "\n")
      when panOpts.trace       $ putTerm (logSourceFloor <> "\n")

      whenJust traceFile hClose

      return x
 
 where
  prettyDiagnostic :: Diagnostic a => DiagnosticEnvelope a -> Doc
  prettyDiagnostic diagEnv = 
    let msg = diagnosticMessage diagEnv.diagnostic
    in case diagEnv.severity of
      SevError -> prettyErrorDiagnostic msg diagEnv.provenance
      SevWarning -> ann Error "warning:" <+> align msg
      SevInfo -> logSource diagEnv.rapporteur <+> align msg
      SevTrace ->
        logSource diagEnv.rapporteur <\\>
        logSourceFloor <\\>
        msg <\\>
        logSourceCeiling

  logSource :: String -> Doc
  logSource src = ann Margin $ pretty @String $ printf "│ %-16s │" src
 
  logSourceCeiling, logSourceFloor :: Doc
  logSourceCeiling = ann Margin ("╭──────────────────╮")
  logSourceFloor   = ann Margin ("╰──────────────────╯")

-------------------------------------------------------------------------------

getPutDoc :: PanOptions -> Handle -> IO (Doc -> IO ())
getPutDoc panOpts h = do
  o <- renderOptionsForHandle panOpts h
  return $ \d -> Text.hPutStr h (renderDoc o d)

hPutDoc :: PanOptions -> Handle -> Doc -> IO ()
hPutDoc o h d = getPutDoc o h >>= ($ d)

renderOptionsForHandle :: PanOptions -> Handle -> IO RenderOptions
renderOptionsForHandle panOpts h = do
  isTerm <- hIsTerminalDevice h
  supportsColor <- if isTerm then hSupportsANSIColor h else pure False
  return RenderOptions
    { styling    = pureIf (panOpts.color && supportsColor) defaultStyling
    , PP.unicode = panOpts.unicode
    , fixedWidth = if isTerm then panOpts.termWidth else Nothing
    }
