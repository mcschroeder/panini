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
import Panini.Elab.Environment
import Panini.Elab.Error
import Panini.Elab.Module
import Panini.Monad
import Panini.Pretty as PP
import Panini.SMT.Z3
import Panini.Solver.Error
import Prelude
import System.Console.Terminal.Size
import System.Environment
import System.Exit
import System.IO

-------------------------------------------------------------------------------

main :: IO ()
main = do
  panOpts0 <- execParser opts
  -- TODO: check if terminal/stderr supports colors
  noColor <- maybe False (not . null) <$> lookupEnv "NO_COLOR"
  termWidth <- fmap width <$> size
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
  result <- withDiagnosticLogger panOpts $ \logger -> do
    let panState0 = defaultState 
          { diagnosticHandler = logger
          , Panini.Monad.smtTimeout = panOpts.smtTimeout 
          , Panini.Monad.regexTimeout = panOpts.regexTimeout
          , Panini.Monad.debugTraceFrontendGraph = panOpts.debugTraceFrontendGraph
          }
    -- TODO: add source lines for <stdin>
    runPan panState0 $ do
      smtInit ?? (ElabError . SolverError . SmtEvent)
      logRegexInfo
      moduleOrigin <- case panOpts.inputFile of
        Nothing -> Stdin <$> tryIO Text.getContents ?? AppIOError
        Just fp -> return $ File fp
      module_ <- loadModule panOpts moduleOrigin
      maybeSavePanFile panOpts module_
      elaborate module_ ?? ElabError
      vsep . map pretty . getSolvedTypes <$> gets environment

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
