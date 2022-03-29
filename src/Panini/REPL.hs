{-# LANGUAGE OverloadedStrings #-}

module Panini.REPL
  ( repl,
    replSettings,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Char (isSpace, toLower)
import Data.List (isPrefixOf)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Panini.Elaborator
import Panini.Error
import Panini.Parser
import Panini.Printer
import Panini.Provenance
import Panini.SMT
import Panini.Syntax
import Panini.TypeChecker
import Prelude
import System.Console.ANSI
import System.Console.Haskeline
import System.IO

-------------------------------------------------------------------------------

-- | Panini REPL.
repl :: InputT Elab ()
repl = do
  let prompt = "Panini> "
  let byeMsg = "byeee 👋"
  x <- fmap (dropWhile isSpace) <$> getInputLine prompt
  case x of
    Nothing -> outputStrLn byeMsg
    Just "" -> repl
    Just input -> case parseCmd input of
      Left err -> outputStrLn err >> repl
      Right cmd -> case cmd of
        Quit        -> outputStrLn byeMsg
        Format s    -> formatInput s      >> repl
        TypeSynth s -> synthesizeType s   >> repl
        Load fs     -> loadFiles fs       >> repl
        Eval term   -> evaluateInput term >> repl
        Show        -> showState          >> repl
        Forget xs   -> forgetVars xs      >> repl

formatInput :: String -> InputT Elab ()
formatInput input = do
  case parseTerm "<repl>" $ Text.pack input of
    Left err -> do
      err' <- liftIO $ updatePV (addSourceLinesREPL input) err
      outputPretty err'
    Right e -> outputPretty e

synthesizeType :: String -> InputT Elab ()
synthesizeType input = do
  g <- lift $ gets pan_types
  case synth g =<< parseInput input of
    Left err -> outputPretty err
    Right (vc, t) -> do
      outputPretty vc
      outputPretty t

evaluateInput :: String -> InputT Elab ()
evaluateInput input = do
  res <- lift $ tryError $ elabDecl =<< lift (except $ parseInput input)  
  case res of
    Left err -> do
      err' <- liftIO $ updatePV (addSourceLinesREPL input) err
      outputPretty err'
    Right () -> return ()

loadFiles :: [FilePath] -> InputT Elab ()
loadFiles fs = forM_ fs $ \f -> do
  src <- liftIO $ Text.readFile f
  case parseProg f src of
    Left err1 -> outputPretty err1
    Right prog -> do
      res <- lift $ tryError $ elabProg prog
      case res of
        Left err2 -> do
          err2' <- liftIO $ updatePV addSourceLines err2
          outputPretty err2'
        Right () -> return ()

showState :: InputT Elab ()
showState = do
  ElabState{pan_types, pan_terms, pan_vcs} <- lift get
  outputStrLn "\ntyping context"
  mapM_ outputPretty $ Map.toList pan_types
  outputStrLn "\nterm context"
  mapM_ outputPretty $ Map.toList pan_terms
  outputStrLn "\nverification conditions"
  -- mapM_ outputPretty $ Map.toList pan_vcs
  forM_ (Map.toList pan_vcs) $ \(x,vc) -> do
    outputPretty x
    outputPretty vc
    outputPretty $ printSMTLib2 vc

forgetVars :: [String] -> InputT Elab ()
forgetVars xs = do
  forM_ xs $ \x -> do
    let n = Name (Text.pack x) NoPV
    lift $ modify' $ \s -> s { pan_types = Map.delete n s.pan_types }
    lift $ modify' $ \s -> s { pan_types = Map.delete n s.pan_types }
    lift $ modify' $ \s -> s { pan_types = Map.delete n s.pan_types }


-- TODO: number repl lines
-- TODO: error source /= var name source (previous vs current definition)

-------------------------------------------------------------------------------

data Command
  = Quit
  | Format String
  | TypeSynth String
  | Eval String
  | Load [String]
  | Show
  | Forget [String]
  deriving stock (Show, Read)

parseCmd :: String -> Either String Command
parseCmd (':' : input) = case break isSpace input of
  (map toLower -> cmd, dropWhile isSpace -> args)
    | cmd `isPrefixOf` "quit" -> Right Quit
    | cmd `isPrefixOf` "format" -> Right (Format args)
    | cmd `isPrefixOf` "type" -> Right (TypeSynth args)
    | cmd `isPrefixOf` "load" -> Right $ Load (words args)
    | cmd `isPrefixOf` "show" -> Right Show
    | cmd == "forget"         -> Right $ Forget (words args)
    | otherwise -> Left ("unknown command :" ++ cmd)
parseCmd input = Right (Eval input)

-------------------------------------------------------------------------------

-- | Panini REPL settings with working autocomplete.
replSettings :: Maybe FilePath -> Settings Elab
replSettings histFile =
  Settings
    { complete = autocomplete,
      historyFile = histFile,
      autoAddHistory = True
    }

autocomplete :: CompletionFunc Elab
autocomplete = completeWord' Nothing isSpace $ \str -> do
  if null str
    then return []
    else return $ map simpleCompletion $ filter (str `isPrefixOf`) cmds
  where
    cmds = [":quit", ":format", ":type", ":load", ":show"]

-------------------------------------------------------------------------------

class Inputable a where
  parseInput :: String -> Either Error a

instance Inputable Term where
  parseInput = parseTerm "<repl>" . Text.pack

instance Inputable Decl where
  parseInput = parseDecl "<repl>" . Text.pack

-------------------------------------------------------------------------------

addSourceLinesREPL :: String -> PV -> IO PV
addSourceLinesREPL input (FromSource loc Nothing)
  | loc.file == "<repl>" = pure $ FromSource loc (Just $ Text.pack input)
addSourceLinesREPL input (Derived pv x) = do
  pv' <- addSourceLinesREPL input pv
  return $ Derived pv' x
addSourceLinesREPL _ pv = addSourceLines pv

outputPretty :: Pretty a => a -> InputT Elab ()
outputPretty x = do
  ansiColors <- liftIO $ hSupportsANSIColor stdout
  fixedWidth <- liftIO $ fmap snd <$> getTerminalSize
  let opts = RenderOptions {unicodeSymbols = True, ansiColors, fixedWidth}
  outputStrLn $ Text.unpack $ renderDoc opts $ pretty x
