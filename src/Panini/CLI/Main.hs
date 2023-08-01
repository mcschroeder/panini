module Panini.CLI.Main where

import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Maybe
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Options.Applicative
import Panini.CLI.REPL
import Panini.Diagnostics
import Panini.Elab
import Panini.Monad
import Panini.Parser
import Panini.Pretty.Printer as PP
import Panini.Provenance
import Prelude
import System.Console.ANSI
import System.Console.Haskeline
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO

-------------------------------------------------------------------------------

data PanOptions = PanOptions
  { inputFile :: Maybe FilePath
  , noInput :: Bool
  , outputFile :: Maybe FilePath
  , trace :: Bool
  , color :: Bool
  , unicode :: Bool
  }

opts :: ParserInfo PanOptions
opts = info 
  (panOptions <**> helper <**> simpleVersioner "v0.1") 
  (fullDesc <> progDesc "Grammar Inference for Ad Hoc Parsers" <> footer 
    "If no INPUT file is given and stdin is not an interactive terminal,\
    \ or the --no-input flag is passed, then the input will be read from\
    \ stdin."
  )
  where
    panOptions = PanOptions
      <$> (optional $ strArgument $ 
            metavar "INPUT" <> 
            help "Input file (default: stdin)"
          )
      <*> (switch $
            long "no-input" <>
            help "Don't prompt or do anything interactive (disables REPL)"
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
      if isTerm && not panOpts.noInput
        then replMain panOpts
        else batchMain panOpts

getTermRenderOptions :: PanOptions -> IO RenderOptions
getTermRenderOptions panOpts = do
  termWidth <- fmap snd <$> getTerminalSize
  return RenderOptions
    { styling = pureIf panOpts.color defaultStyling
    , PP.unicode = panOpts.unicode
    , fixedWidth = termWidth
    }


replMain :: PanOptions -> IO ()
replMain panOpts = do
  configDir <- getXdgDirectory XdgConfig "panini"
  createDirectoryIfMissing True configDir
  let historyFile = configDir </> "repl_history"
  let replConf = replSettings (Just historyFile)
  let panState = defaultState
  renderOpts <- getTermRenderOptions panOpts
  res <- runPan panState $ runInputT replConf $ do
    whenJust panOpts.outputFile $ \_ ->
      outputStrLn $ "Warning: --output ignored during REPL session"
    when panOpts.trace $ do      
      replPrint <- getExternalPrint
      let logDiag d = replPrint $ Text.unpack (renderDoc renderOpts $ prettyDiagnostic d) ++ "\n"
      lift $ modify' (\s -> s { logDiagnostic = logDiag })
    repl
  case res of
    Left err -> do
      putStrLn $ "panic! at the repl: " ++ show err -- TODO: pretty?
      exitFailure
    Right _ -> return ()

batchMain :: PanOptions -> IO ()
batchMain panOpts = do
  renderOpts <- getTermRenderOptions panOpts
  let logDiag = Text.hPutStrLn stderr . renderDoc renderOpts . prettyDiagnostic
  let panState = defaultState 
        { logDiagnostic = if panOpts.trace then logDiag else const (return ())
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

