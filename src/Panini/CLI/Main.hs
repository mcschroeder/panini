module Panini.CLI.Main where

import Control.Exception
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Maybe
import Data.Text qualified as Text
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

main :: IO ()
main = do
  panOpts0 <- execParser opts
  
  -- TODO: check if terminal/stderr supports colors
  noColor <- maybe False (not . null) <$> lookupEnv "NO_COLOR"
  let panOpts = panOpts0 { color = panOpts0.color && not noColor }
  
  traceFileH <- case panOpts.traceFile of
    Nothing -> return Nothing
    Just fp -> do
      h <- openFile fp WriteMode
      hSetBuffering h NoBuffering
      return $ Just h

  isTerm <- hIsTerminalDevice stdin
  let mainFunc 
        | isJust panOpts.inputFile      = batchMain
        | isTerm && not panOpts.noInput = replMain
        | otherwise                     = batchMain  -- stdin
  
  mainFunc panOpts traceFileH `finally` whenJust traceFileH hClose

replMain :: PanOptions -> Maybe Handle -> IO ()
replMain panOpts traceFileH = do
  configDir <- getXdgDirectory XdgConfig "panini"
  createDirectoryIfMissing True configDir
  let historyFile = configDir </> "repl_history"
  let replConf = replSettings (Just historyFile)
  let panState = defaultState
  res <- runPan panState $ runInputT replConf $ do
    whenJust panOpts.outputFile $ \_ ->
      outputStrLn $ "Warning: --output ignored during REPL session"
    log1 <- mkLogFuncREPL panOpts
    log2 <- liftIO $ mkLogFuncFile panOpts traceFileH
    let logDiag = liftM2 (>>) log1 log2
    lift $ modify' (\s -> s { eventHandler = logDiag })
    repl
  case res of
    Left _ -> exitFailure
    Right _ -> return ()

batchMain :: PanOptions -> Maybe Handle -> IO ()
batchMain panOpts traceFileH = do
  log1 <- mkLogFuncTerm panOpts
  log2 <- mkLogFuncFile panOpts traceFileH
  let logDiag = liftM2 (>>) log1 log2
  let panState = defaultState { eventHandler = logDiag }
  res <- runPan panState $ addSourceLinesToError $ do
    module_ <- maybe (pure stdinModule) (liftIO . getModule) panOpts.inputFile
    logMessageDoc "Panini" $ "Read" <+> pretty module_
    src <- if module_ == stdinModule
      then tryIO NoPV $ Text.getContents
      else tryIO NoPV $ Text.readFile $ moduleLocation module_
    logData (moduleLocation module_ ++ " (Raw Source)") src    
    prog <- parseSource (moduleLocation module_) src
    elaborate module_ prog
    return ()
  case res of
    Left _ -> exitFailure
    Right _ -> return ()

-- TODO: duplicate of function in Panini.CLI.REPL
addSourceLinesToError :: Pan a -> Pan a
addSourceLinesToError m = m `catchError` \err ->
  throwError =<< updatePV (liftIO . addSourceLines) err

-------------------------------------------------------------------------------

-- TODO: investigate logging asynchronously via TChan/TQueue

mkLogFuncFile :: PanOptions -> Maybe Handle -> IO (Event -> IO ())
mkLogFuncFile panOpts traceFileH = case traceFileH of
  Nothing -> return $ const $ return ()
  Just h -> do
    let renderOpts = fileRenderOptions panOpts
    return $ Text.hPutStrLn h . renderDoc renderOpts . prettyEvent

mkLogFuncREPL :: MonadIO m => PanOptions -> InputT m (Event -> IO ())
mkLogFuncREPL panOpts = do
  renderOpts <- liftIO $ getTermRenderOptions panOpts
  let renderEvent = renderDoc renderOpts . prettyEvent
  extPrint <- getExternalPrint  
  let putEvent = extPrint . (++ "\n") . Text.unpack . renderEvent
  return $ \case
    ev@(ErrorEvent _)  -> putEvent ev
    ev | panOpts.trace -> putEvent ev
    _                  -> return ()

mkLogFuncTerm :: PanOptions -> IO (Event -> IO ())
mkLogFuncTerm panOpts = do
  renderOpts <- getTermRenderOptions panOpts
  let renderEvent = renderDoc renderOpts . prettyEvent
  let putEvent = Text.hPutStrLn stderr . renderEvent
  return $ \case
    ev@(ErrorEvent _)  -> putEvent ev
    ev | panOpts.trace -> putEvent ev
    _                  -> return ()

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
