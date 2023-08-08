{-# LANGUAGE RecordWildCards #-}
module Panini.CLI.Main where

import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
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
import Panini.SMT.Z3
import Prelude
import System.Console.ANSI
import System.Console.Haskeline
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import Control.Monad.Trans.State.Strict
import Panini.Environment
import Data.Map qualified as Map
import Data.Function
import Data.List qualified as List
import Panini.Syntax.AST
import Panini.Abstract.AString (AString)
import Panini.Syntax
import Data.Generics.Uniplate.Operations

-------------------------------------------------------------------------------

data PanOptions = PanOptions
  { inputFile :: Maybe FilePath
  , noInput :: Bool
  , outputFile :: Maybe FilePath
  , outputGrammars :: Bool
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
            long "grammars" <>
            short 'g' <>
            help "Output only inferred grammars"
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
        lift smtInit
        whenJust panOpts.outputFile $ \_ ->
          outputStrLn $ "Warning: --output ignored during REPL session"
        repl
    
    -- batch mode --------------------------------------------------------------
    else do
      -- TODO: add source lines for <stdin>
      runPan panState0 $ addSourceLinesToError $ do
        smtInit
        module_ <- maybe (pure stdinModule) (liftIO . getModule) panOpts.inputFile
        logMessage $ "Read" <+> pretty module_
        src <- if module_ == stdinModule
          then tryIO NoPV $ Text.getContents
          else tryIO NoPV $ Text.readFile $ moduleLocation module_
        logData src
        prog <- parseSource (moduleLocation module_) src
        elaborate module_ prog
        if panOpts.outputGrammars then
          outputInferredGrammars panOpts
        else
          outputInferredTypes panOpts
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

-- TODO: output these in the REPL as well

outputInferredTypes :: PanOptions -> Pan ()
outputInferredTypes panOpts = do
  env <- gets environment
  let inferredDefs = [ (x,_solvedType) | (x,Verified{..}) <- Map.toList env
                                       , _assumedType /= Just _solvedType ]
  let ts = List.sortBy (compareSrcLoc `on` getPV . fst) inferredDefs
  let doc = vsep $ map (\(x,t) -> pretty x <+> symColon <+> pretty t) ts
  liftIO $ do
    renderOpts <- getTermRenderOptions panOpts
    Text.putStrLn $ renderDoc renderOpts doc

outputInferredGrammars :: PanOptions -> Pan ()
outputInferredGrammars panOpts = do
  env <- gets environment
  let inferredDefs = [ (x,_solvedType) | (x,Verified{..}) <- Map.toList env
                                       , _assumedType /= Just _solvedType ]
  let ts = List.sortBy (compareSrcLoc `on` getPV . fst) inferredDefs
  let gs = concatMap extractGrammars $ map snd ts
  let doc = vsep $ map pretty gs
  liftIO $ do
    renderOpts <- getTermRenderOptions panOpts
    Text.putStrLn $ renderDoc renderOpts doc

extractGrammars :: Type -> [AString]
extractGrammars (TFun _ t1 t2 _) = extractGrammars t1 ++ extractGrammars t2
extractGrammars t@(TBase x TString (Known p) _) = case p of
  PRel (EVar y :∈: EStrA s) | x == y -> [s]
  _ | not $ null [True | PRel (_ :∈: _) <- universe p ] -> 
      error $ "extractGrammars: irregular grammar: " ++ showPretty t
  _ -> []
extractGrammars _ = []

compareSrcLoc :: PV -> PV -> Ordering
compareSrcLoc (Derived pv1 _) pv2 = compareSrcLoc pv1 pv2
compareSrcLoc pv1 (Derived pv2 _) = compareSrcLoc pv1 pv2
compareSrcLoc (FromSource loc1 _) (FromSource loc2 _) = compare loc1 loc2
compareSrcLoc (FromSource _ _) NoPV = GT
compareSrcLoc NoPV (FromSource _ _) = LT
compareSrcLoc NoPV NoPV = EQ

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
