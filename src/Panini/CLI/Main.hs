{-# LANGUAGE RecordWildCards #-}
module Panini.CLI.Main where

import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Function
import Data.Maybe
import Data.Text.IO qualified as Text
import Options.Applicative
import Panini.CLI.Options
import Panini.CLI.REPL
import Panini.CLI.Test
import Panini.Elab
import Panini.Events
import Panini.Modules
import Panini.Monad
import Panini.Parser
import Panini.Pretty as PP
import Panini.Provenance
import Panini.SMT.Z3
import Prelude
import System.Environment
import System.Exit
import System.IO
import System.Console.ANSI

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
      
  let eventHandler ev = do
        whenJust traceFile (putEventFile panOpts ev)
        when (panOpts.trace || isErrorEvent ev) (putEventStderr panOpts ev)

  let panState0 = defaultState 
        { eventHandler
        , Panini.Monad.smtTimeout = panOpts.smtTimeout 
        }

  -- TODO: add source lines for <stdin>
  result <- runPan panState0 $ addSourceLinesToError $ do
    smtInit
    module_ <- maybe (pure stdinModule) (liftIO . getModule) panOpts.inputFile
    logMessage $ "Read" <+> pretty module_
    src <- if module_ == stdinModule
      then tryIO NoPV $ Text.getContents
      else tryIO NoPV $ Text.readFile $ moduleLocation module_
    logData src
    prog <- parseSource (moduleLocation module_) src
    elaborate module_ prog
    if panOpts.outputGrammars 
      then vsep . map pretty <$> getVerifiedGrammars
      else vsep . map pretty <$> getVerifiedTypes
  
  whenJust traceFile hClose

  case result of
    Left  _ -> exitFailure
    Right doc
      | Just outFile <- panOpts.outputFile -> do
          withFile outFile WriteMode $ putDocFile panOpts doc
          exitSuccess

      | otherwise -> do
          putDocStdout panOpts $ doc <> "\n"
          exitSuccess

-- TODO: duplicate of function in Panini.CLI.REPL
addSourceLinesToError :: Pan a -> Pan a
addSourceLinesToError m = m `catchError` \err ->
  throwError =<< updatePV (liftIO . addSourceLines) err
