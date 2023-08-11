{-# LANGUAGE RecordWildCards #-}
module Panini.CLI.Main where

import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Function
import Data.Generics.Uniplate.Operations
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe
import Data.Text.IO qualified as Text
import Options.Applicative
import Panini.Abstract.AString (AString)
import Panini.CLI.Options
import Panini.CLI.REPL
import Panini.CLI.Test
import Panini.Elab
import Panini.Environment
import Panini.Events
import Panini.Modules
import Panini.Monad
import Panini.Parser
import Panini.Pretty.Printer as PP
import Panini.Provenance
import Panini.SMT.Z3
import Panini.Syntax
import Prelude
import System.Environment
import System.Exit
import System.IO

-------------------------------------------------------------------------------

main :: IO ()
main = do
  panOpts0 <- execParser opts

  -- TODO: check if terminal/stderr supports colors
  noColor <- maybe False (not . null) <$> lookupEnv "NO_COLOR"
  let panOpts = panOpts0 { color = panOpts0.color && not noColor }  
  
  if panOpts.testMode 
    then testMain panOpts
    else do
      isTerm <- hIsTerminalDevice stdin
      if isNothing panOpts.inputFile && isTerm && not panOpts.noInput
        then replMain panOpts
        else batchMain panOpts

batchMain :: PanOptions -> IO ()
batchMain panOpts = do
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

    outputter <- liftIO $ mkOutputter panOpts
    outdoc <- if panOpts.outputGrammars 
                then getPrettyInferredGrammars
                else getPrettyInferredTypes
    liftIO $ outputter outdoc
  
  whenJust traceFileHandle hClose

  case result of
    Left  _ -> exitFailure
    Right _ -> exitSuccess

-- TODO: duplicate of function in Panini.CLI.REPL
addSourceLinesToError :: Pan a -> Pan a
addSourceLinesToError m = m `catchError` \err ->
  throwError =<< updatePV (liftIO . addSourceLines) err

-------------------------------------------------------------------------------

mkOutputter :: PanOptions -> IO (Doc -> IO ())
mkOutputter panOpts = case panOpts.outputFile of
  Just fp -> do
    let renderOpts = fileRenderOptions panOpts
    return $ \doc -> withFile fp WriteMode $ \h ->
      Text.hPutStr h $ renderDoc renderOpts doc
  Nothing -> do
    renderOpts <- getTermRenderOptions panOpts
    return $ \doc -> 
      Text.putStrLn $ renderDoc renderOpts doc

-- TODO: output these in the REPL as well

getPrettyInferredTypes :: Pan Doc
getPrettyInferredTypes = do
  env <- gets environment
  let inferredDefs = [ (x,_solvedType) | (x,Verified{..}) <- Map.toList env
                                       , _assumedType /= Just _solvedType ]
  let ts = List.sortBy (compare `on` getPV . fst) inferredDefs
  return $ vsep $ map (\(x,t) -> pretty x <+> symColon <+> pretty t) ts

getPrettyInferredGrammars :: Pan Doc
getPrettyInferredGrammars = do
  env <- gets environment
  let inferredDefs = [ (x,_solvedType) | (x,Verified{..}) <- Map.toList env
                                       , _assumedType /= Just _solvedType ]
  let ts = List.sortBy (compare `on` getPV . fst) inferredDefs
  let gs = concatMap extractGrammars $ map snd ts
  return $ vsep $ map pretty gs

-- TODO: this is pretty hacky and limited
extractGrammars :: Type -> [AString]
extractGrammars (TFun _ t1 t2 _) = extractGrammars t1 ++ extractGrammars t2
extractGrammars t@(TBase x TString (Known p) _) = case p of
  PRel (EVar y :∈: EStrA s) | x == y -> [s]
  _ | not $ null [True | PRel (_ :∈: _) <- universe p ] -> 
      error $ "extractGrammars: irregular grammar: " ++ showPretty t
  _ -> []
extractGrammars _ = []
