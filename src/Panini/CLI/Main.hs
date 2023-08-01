module Panini.CLI.Main where

import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Maybe
import Data.Text qualified as Text
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
import System.Environment
import System.Exit
import System.FilePath
import System.IO

-------------------------------------------------------------------------------

data PanOptions = PanOptions
  { inputFile :: Maybe FilePath
  , outputFile :: Maybe FilePath
  , trace :: Bool
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
            long "trace" <> 
            help "Show detailed diagnostics and debugging information"
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
  panOpts0 <- execParser opts
  
  -- TODO: check if terminal/stderr supports colors
  noColor <- maybe False (not . null) <$> lookupEnv "NO_COLOR"
  let panOpts = panOpts0 { color = panOpts0.color && not noColor }
  
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
        { colorOutput = panOpts.color
        , unicodeOutput = panOpts.unicode
        }
  res <- runPan panState $ runInputT replConf $ do
    whenJust panOpts.outputFile $ \_ ->
      outputStrLn $ "Warning: --output ignored during REPL session"
    when panOpts.trace $ do      
      replPrint <- getExternalPrint
      let termPrint s = replPrint $ Text.unpack s ++ "\n"
      lift $ modify' (\s -> s { logTermPrint = Just termPrint })
    repl
  case res of
    Left err -> do
      putStrLn $ "panic! at the repl: " ++ show err -- TODO: pretty?
      exitFailure
    Right _ -> return ()

batchMain :: PanOptions -> IO ()
batchMain panOpts = do
  let panState = defaultState
        { colorOutput = panOpts.color
        , unicodeOutput = panOpts.unicode        
        }
  res <- runPan panState $ do
    when panOpts.trace $ 
      modify' (\s -> s { logTermPrint = Just (Text.hPutStrLn stderr) })
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

