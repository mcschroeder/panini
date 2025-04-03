module Panini.CLI.Common where

import Control.Monad.Extra
import Data.Maybe
import Data.Text.IO qualified as Text
import Panini.CLI.Error
import Panini.CLI.Options
import Panini.Diagnostic
import Panini.Elab.Error qualified as Elab
import Panini.Elab.Module
import Panini.Frontend.Python
import Panini.Monad
import Panini.Parser (parseProgram)
import Panini.Pretty as PP
import Panini.Provenance
import Panini.SMT.Z3
import Panini.Solver.Error qualified as Solver
import Prelude
import System.Console.ANSI
import System.FilePath
import System.IO
import System.Time.Extra

-------------------------------------------------------------------------------

initialize :: Pan AppError ()
initialize = do
  smtInit ?? (ElabError . Elab.SolverError . Solver.SmtError)
  t <- showDuration <$> gets Panini.Monad.regexTimeout
  info $ "Regex simplifier timeout:" <+> pretty t

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
  | inputIsPan = info @Doc $ "Warning:" <+> warnIgnore
  | otherwise = do
      let pan = mkPanFile module_.moduleOrigin
      let opt = RenderOptions Nothing True Nothing
      let src = renderDoc opt $ pretty module_.program
      info $ "Writing transpiled source to" <+> pretty pan
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
withDiagnosticLogger panOpts m = do
  -- TODO: get trace file as CLI option
  traceFile <- whenMaybe panOpts.traceToFile
                (openLogFileFor $ fromMaybe "stdin" panOpts.inputFile)            

  putFile <- maybe (return $ \_ -> pure ()) (getPutDoc panOpts) traceFile  
  putTerm <- getPutDoc panOpts stderr

  let putLog d = do putFile (d <> "\n")
                    when panOpts.trace $ putTerm (d <> "\n")
  
  let suppressErrors = panOpts.testMode && not panOpts.trace

  let putErr d = do putFile (d <> "\n")
                    unless suppressErrors $ putTerm (d <> "\n")
  
  x <- m $ \ev0 -> do
    pv' <- addSourceLines ev0.provenance
    let ev = ev0 { provenance = pv' }
    let src = ann Margin $ "(" <> pretty ev.rapporteur <> ")"
    let msg = diagnosticMessage ev.diagnostic
    let errMsg = prettyErrorDiagnostic msg ev.provenance
    case ev.severity of
      SevInfo    -> putLog $ src <+> msg
      SevTrace   -> putLog msg
      SevWarning -> putErr msg
      SevError   -> putErr errMsg

  whenJust traceFile hClose
  return x
   
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

-------------------------------------------------------------------------------

panStateWithOptions :: PanOptions -> PanState
panStateWithOptions o = defaultState
  { Panini.Monad.smtTimeout = o.smtTimeout 
  , Panini.Monad.regexTimeout = o.regexTimeout
  , Panini.Monad.debugTraceFrontendGraph = o.debugTraceFrontendGraph
  }
