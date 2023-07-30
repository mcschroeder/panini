module Panini.CLI.Main where

import Control.Monad
import Data.Maybe
import Data.Text.IO qualified as Text
import Options.Applicative
import Panini.CLI.REPL
import Panini.Elab
import Panini.Logger
import Panini.Monad
import Panini.Parser
import Panini.Pretty.Printer
import Panini.Provenance
import Prelude
import System.Console.Haskeline
import System.Directory
import System.Exit
import System.FilePath
import System.IO

-------------------------------------------------------------------------------

data PanOptions = PanOptions
  { inputFile :: Maybe FilePath
  , outputFile :: Maybe FilePath
  , debug :: Bool
  , color :: Bool
  , unicode :: Bool
  }

opts :: ParserInfo PanOptions
opts = info (panOptions <**> helper <**> simpleVersioner "v0.1") 
            (fullDesc <> progDesc "Grammar Inference for Ad Hoc Parsers")
  where
    panOptions = PanOptions
      <$> (optional $ strArgument $ 
            metavar "INPUT" <> 
            help "Input file (default: stdin)"
          )
      <*> (optional $ strOption $ 
            long "output" <> 
            short 'o' <> 
            metavar "FILE" <> 
            help "Write output to FILE (default: stdout)"
          )
      <*> (switch $ 
            long "debug" <> 
            help "Show debugging output"
          )
      <*> (flag True False $ 
            long "no-color" <> 
            help "Disable color output to terminal"
          )
      <*> (flag True False $ 
            long "no-unicode" <> 
            help "Disable Unicode output to terminal and files"
          )

main :: IO ()
main = do
  panOpts <- execParser opts
  case panOpts.inputFile of
    Just _ -> batchMain panOpts
    Nothing -> do
      isTerm <- hIsTerminalDevice stdin
      if isTerm
        then replMain panOpts
        else batchMain panOpts

replMain :: PanOptions -> IO ()
replMain panOpts = do
  configDir <- getXdgDirectory XdgConfig "panini"
  createDirectoryIfMissing True configDir
  let historyFile = configDir </> "repl_history"
  let replConf = replSettings (Just historyFile)
  let panState = defaultState
        { debugMode = panOpts.debug
        , colorOutput = panOpts.color
        , unicodeOutput = panOpts.unicode 
        }
  when (isJust panOpts.outputFile) $
    putStrLn $ "Warning: --output is ignored during a REPL session"
  res <- runPan panState $ runInputT replConf repl
  case res of
    Left err -> do
      putStrLn $ "panic! at the repl: " ++ show err -- TODO: pretty?
      exitFailure
    Right _ -> return ()

-- TODO: output errors and log traces to stderr
batchMain :: PanOptions -> IO ()
batchMain panOpts = do
  let panState = defaultState
        { debugMode = panOpts.debug
        , colorOutput = panOpts.color
        , unicodeOutput = panOpts.unicode
        }
  res <- runPan panState $ do
    (path,src) <- case panOpts.inputFile of
      Just path -> do
        logMessage "Panini" $ "Read " ++ path
        src <- tryIO NoPV $ Text.readFile path
        logData (path ++ " (Raw Source)") src
        return (path,src)
      Nothing -> do
        logMessage "Panini" $ "Read stdin"
        src <- tryIO NoPV $ Text.getContents
        logData ("<stdin> (Raw Source)") src
        return ("<stdin>", src)
    prog <- parseSource path src
    elaborateProgram path prog
    return ()
  case res of
    Left err -> do
      putStrLn $ showPretty err  -- TODO
      exitFailure
    Right _ -> return ()

