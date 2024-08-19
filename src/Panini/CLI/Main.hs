{-# LANGUAGE RecordWildCards #-}
module Panini.CLI.Main where

import Control.Monad.Extra
import Data.Function
import Data.Maybe
import Data.Text.IO qualified as Text
import Options.Applicative
import Panini.CLI.Common
import Panini.CLI.Options
import Panini.CLI.REPL
import Panini.CLI.Test
import Panini.Elab
import Panini.Environment
import Panini.Events
import Panini.Frontend.Python
import Panini.Monad
import Panini.Pretty as PP
import Panini.Provenance
import Panini.SMT.Z3
import Prelude
import System.Console.ANSI
import System.Environment
import System.Exit
import System.FilePath
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
      
  let eventHandler ev0 = do
        ev <- updatePV addSourceLines ev0
        whenJust traceFile (putEventFile panOpts ev)
        when (panOpts.trace || isErrorEvent ev) (putEventStderr panOpts ev)

  let panState0 = defaultState 
        { eventHandler
        , Panini.Monad.smtTimeout = panOpts.smtTimeout 
        }

  -- TODO: add source lines for <stdin>
  result <- runPan panState0 $ do
    smtInit    
    let fp = fromMaybe "<stdin>" panOpts.inputFile
    logMessage $ "Read" <+> pretty fp
    src <- tryIO NoPV $ maybe Text.getContents Text.readFile panOpts.inputFile
    let ext = maybe ".pan" takeExtension panOpts.inputFile
    let loadFunc | panOpts.pythonInput || ext == ".py" = loadModulePython
                 | otherwise                           = loadModule
    (module_, prog) <- loadFunc src fp
    elaborate module_ prog
    vsep . map pretty . getSolvedTypes <$> gets environment

  whenJust traceFile hClose

  case result of
    Left  _ -> exitFailure
    Right (doc,panState1) -> do
      case panOpts.outputFile of
        Just outFile -> withFile outFile WriteMode $ putDocFile panOpts doc
        Nothing -> putDocStdout panOpts $ doc <> "\n"
      if null $ getTypeErrors panState1.environment
        then exitSuccess
        else exitFailure
