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
import Control.Monad.IO.Class
import Control.Exception

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
    lift $ modify' (\s -> s { logDiagnostic = logDiag })
    repl
  case res of
    Left err -> do
      putStrLn $ "panic! at the repl: " ++ show err -- TODO: pretty?
      exitFailure
    Right _ -> return ()

batchMain :: PanOptions -> Maybe Handle -> IO ()
batchMain panOpts traceFileH = do
  log1 <- mkLogFuncTerm panOpts
  log2 <- mkLogFuncFile panOpts traceFileH
  let logDiag = liftM2 (>>) log1 log2
  let panState = defaultState { logDiagnostic = logDiag }
  res <- runPan panState $ addSourceLinesToError $ do
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


-- TODO: duplicate of function in Panini.CLI.REPL
addSourceLinesToError :: Pan a -> Pan a
addSourceLinesToError m = m `catchError` \err ->
  throwError =<< updatePV (liftIO . addSourceLines) err

-------------------------------------------------------------------------------

-- TODO: always log errors unless --quiet

-- TODO: investigate logging asynchronously via TChan/TQueue

mkLogFuncFile :: PanOptions -> Maybe Handle -> IO (Diagnostic -> IO ())
mkLogFuncFile panOpts traceFileH = case traceFileH of
  Nothing -> return $ const $ return ()
  Just h -> do
    let renderOpts = fileRenderOptions panOpts
    return $ Text.hPutStrLn h . renderDoc renderOpts . prettyDiagnostic

mkLogFuncREPL :: MonadIO m => PanOptions -> InputT m (Diagnostic -> IO ())
mkLogFuncREPL panOpts = case panOpts.trace of
  False -> return $ const $ return ()
  True -> do
    renderOpts <- liftIO $ getTermRenderOptions panOpts
    extPrint <- getExternalPrint
    return $ extPrint . (++ "\n") . Text.unpack . renderDoc renderOpts . prettyDiagnostic

mkLogFuncTerm :: PanOptions -> IO (Diagnostic -> IO ())
mkLogFuncTerm panOpts = case panOpts.trace of
  False -> return $ const $ return ()
  True -> do
    renderOpts <- getTermRenderOptions panOpts
    return $ Text.hPutStrLn stderr . renderDoc renderOpts . prettyDiagnostic

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
