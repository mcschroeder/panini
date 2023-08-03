module Panini.CLI.Main where

import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text.IO qualified as Text
import Options.Applicative
import Panini.CLI.REPL
import Panini.Elab
import Panini.Events
import Panini.Modules
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
  , traceFile :: Maybe FilePath
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
      <*> (optional $ strOption $
            long "trace-file" <>
            metavar "FILE" <>
            help "Write debugging information to FILE"
          )
      <*> (flag True False $ 
            long "no-color" <> 
            help "Disable color output to terminal"
          )
      <*> (flag True False $ 
            long "no-unicode" <> 
            help "Disable Unicode output to terminal and files"
          )

-------------------------------------------------------------------------------

-- TODO: write output to file

main :: IO ()
main = do
  panOpts0 <- execParser opts
  
  -- TODO: check if terminal/stderr supports colors
  noColor <- maybe False (not . null) <$> lookupEnv "NO_COLOR"
  let panOpts = panOpts0 { color = panOpts0.color && not noColor }
  
  traceFileHandle <- forM panOpts.traceFile $ \fp -> do
      h <- openFile fp WriteMode
      hSetBuffering h NoBuffering
      return h
      
  let eventHandler ev = do
        whenJust traceFileHandle $ \h -> do
          let fileRenderOpts = fileRenderOptions panOpts
          Text.hPutStrLn h $ renderDoc fileRenderOpts $ prettyEvent ev
        when (panOpts.trace || isErrorEvent ev) $ do
          termRenderOpts <- liftIO $ getTermRenderOptions panOpts
          Text.hPutStrLn stderr $ renderDoc termRenderOpts $ prettyEvent ev

  let panState0 = defaultState { eventHandler }

  isTerm <- hIsTerminalDevice stdin  
  result <- if isNothing panOpts.inputFile && isTerm && not panOpts.noInput
    -- REPL mode --------------------------------------------------------------
    then do
      configDir <- getXdgDirectory XdgConfig "panini"
      createDirectoryIfMissing True configDir
      let historyFile = configDir </> "repl_history"
      let replConf = replSettings (Just historyFile)
      runPan panState0 $ runInputT replConf $ do
        whenJust panOpts.outputFile $ \_ ->
          outputStrLn $ "Warning: --output ignored during REPL session"
        repl
    
    -- batch mode --------------------------------------------------------------
    else do
      runPan panState0 $ addSourceLinesToError $ do
        module_ <- maybe (pure stdinModule) (liftIO . getModule) panOpts.inputFile
        logMessageDoc "Panini" $ "Read" <+> pretty module_
        src <- if module_ == stdinModule
          then tryIO NoPV $ Text.getContents
          else tryIO NoPV $ Text.readFile $ moduleLocation module_
        logData (moduleLocation module_ ++ " (Raw Source)") src    
        prog <- parseSource (moduleLocation module_) src
        elaborate module_ prog
        return ()
  
  whenJust traceFileHandle hClose

  case result of
    Left  _ -> exitFailure
    Right _ -> exitSuccess


-- TODO: duplicate of function in Panini.CLI.REPL
addSourceLinesToError :: Pan a -> Pan a
addSourceLinesToError m = m `catchError` \err ->
  throwError =<< updatePV (liftIO . addSourceLines) err

-------------------------------------------------------------------------------

getTermRenderOptions :: PanOptions -> IO RenderOptions
getTermRenderOptions panOpts = do
  termWidth <- fmap snd <$> getTerminalSize
  return RenderOptions
    { styling = pureIf panOpts.color defaultStyling
    , PP.unicode = panOpts.unicode
    , fixedWidth = termWidth
    }

fileRenderOptions :: PanOptions -> RenderOptions
fileRenderOptions panOpts = RenderOptions 
  { styling = Nothing
  , PP.unicode = panOpts.unicode
  , fixedWidth = Nothing
  }
