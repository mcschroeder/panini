{-# LANGUAGE RecordWildCards #-}
module Panini.CLI.Main where

import Control.Monad.Extra
import Data.Function
import Data.Maybe
import Data.Text.IO qualified as Text
import Options.Applicative
import Panini.CLI.Common
import Panini.CLI.Error
import Panini.CLI.Options
import Panini.CLI.REPL
import Panini.CLI.Test
import Panini.Diagnostic
import Panini.Elab
import Panini.Environment
import Panini.Frontend.Python
import Panini.Monad
import Panini.Pretty as PP
import Panini.Provenance
import Panini.SMT.Z3
import Panini.Solver.Error
import Prelude
import System.Console.ANSI
import System.Environment
import System.Exit
import System.IO

-------------------------------------------------------------------------------

main :: IO ()
main = do
  panOpts0 <- execParser opts
  -- TODO: check if terminal/stderr supports colors
  noColor <- maybe False (not . null) <$> lookupEnv "NO_COLOR"  
  termWidth <- fmap snd <$> getTerminalSize
  let panOpts = panOpts0 
        { color = panOpts0.color && not noColor
        , termWidth = panOpts0.termWidth <|> termWidth
        }
  
  if panOpts.testMode 
    then testMain panOpts
    else do
      isTerm <- hIsTerminalDevice stdin
      if isNothing panOpts.inputFile && isTerm && not panOpts.noInput
        then replMain panOpts
        else batchMain panOpts

batchMain :: PanOptions -> IO ()
batchMain panOpts = do
  traceFile <- whenMaybe panOpts.traceToFile 
                (openLogFileFor $ fromMaybe "stdin" panOpts.inputFile)
      
  let diagnosticHandler ev0 = do
        pv' <- addSourceLines ev0.provenance
        let ev = ev0 { provenance = pv' }
        whenJust traceFile (putDiagnosticFile panOpts ev)
        when (panOpts.trace || isError ev) (putDiagnosticStderr panOpts ev)

  let panState0 = defaultState 
        { diagnosticHandler
        , Panini.Monad.smtTimeout = panOpts.smtTimeout 
        , Panini.Monad.regexTimeout = panOpts.regexTimeout
        , Panini.Monad.debugTraceFrontendGraph = panOpts.debugTraceFrontendGraph
        }

  -- TODO: add source lines for <stdin>
  result <- runPan panState0 $ do
    smtInit ?? (ElabError . SolverError . SmtError)
    logRegexInfo
    let fp = fromMaybe "<stdin>" panOpts.inputFile
    logMessage $ "Read" <+> pretty fp
    src <- tryIO (maybe Text.getContents Text.readFile panOpts.inputFile) ?? AppIOError
    (module_, prog) <- case determineFileType panOpts fp of
      PythonSource -> loadModulePython src fp
      PaniniSource -> loadModule src fp
    maybeSavePanFile panOpts module_ prog
    elaborate module_ prog ?? ElabError
    vsep . map pretty . getSolvedTypes <$> gets environment

  whenJust traceFile hClose

  case result of
    Left _ -> exitFailure
    Right (doc,panState1) -> do
      case panOpts.outputFile of
        Just outFile -> withFile outFile WriteMode $ putDocFile panOpts doc
        Nothing -> putDocStdout panOpts $ doc <> "\n"
      case getTypeErrors panState1.environment of
        [] -> exitSuccess
        xs -> do
          putDocStderr panOpts (vsep (map prettyError xs) <> "\n")
          exitFailure
