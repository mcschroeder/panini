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
import Panini.Checker
import Panini.Elaborator
import Panini.Parser
import Panini.Printer
import Panini.Syntax
import System.Console.ANSI
import System.Console.Haskeline
import System.IO
import Prelude

-------------------------------------------------------------------------------

-- | Panini REPL.
repl :: InputT Panini ()
repl = do
  let prompt = "Panini> "
  let byeMsg = "byeee 👋"
  x <- getInputLine prompt
  case x of
    Nothing -> outputStrLn byeMsg
    Just input -> case parseCmd input of
      Left err -> outputStrLn err >> repl
      Right cmd -> case cmd of
        Quit -> outputStrLn byeMsg
        Format args -> format args >> repl
        TypeCheck args -> typeCheck args >> repl
        Eval arg -> evalDecl arg >> repl
        Load args -> loadModules args >> repl

format :: String -> InputT Panini ()
format input =
  case parseExpr "<repl>" (Text.pack input) of
    Left err -> outputStrLn err
    Right ex -> outputExpr ex

typeCheck :: String -> InputT Panini ()
typeCheck input =
  case parseExpr "<repl>" (Text.pack input) of
    Left err1 -> outputStrLn err1
    Right e -> do
      opts <- getPrintOptions
      let g0 = Map.empty      
      case synth g0 e of
        Left err2 -> do
          outputStrLn ""
          outputStrLn $ Text.unpack $ printError opts "<repl>" err2
        Right (c, t) -> do
          outputStrLn $ Text.unpack $ printCon opts c
          outputStrLn ""
          outputStrLn $ Text.unpack $ printType opts t

evalDecl :: String -> InputT Panini ()
evalDecl input =
  case parseDecl "<repl>" (Text.pack input) of
    Left err1 -> outputStrLn err1
    Right decl -> do
      ps <- lift get
      res <- liftIO $ runExceptT $ execStateT (elabDecl decl) ps
      case res of
        Left err2 -> do
          opts <- getPrintOptions
          outputStrLn $ Text.unpack $ printError opts "<repl>" err2
        Right ps' -> do
          lift $ put ps'

-- TODO: add state to Panini monad
loadModules :: [String] -> InputT Panini ()
loadModules ms = forM_ ms $ \m -> do
  src <- liftIO $ Text.readFile m
  case parseProg m src of
    Left err1 -> outputStrLn err1
    Right prog -> do
      -- lift $ elabProg prog
      opts <- getPrintOptions
      outputStrLn $ Text.unpack $ printProg opts prog
      outputStrLn ""
  return ()

outputExpr :: Expr -> InputT Panini ()
outputExpr e = do
  opts <- getPrintOptions
  let t = printExpr opts e
  outputStrLn $ Text.unpack t

getPrintOptions :: InputT Panini PrintOptions
getPrintOptions = liftIO $ do
  ansiColors <- hSupportsANSIColor stdout
  fixedWidth <- fmap snd <$> getTerminalSize
  return PrintOptions {unicodeSymbols = True, ansiColors, fixedWidth}

-------------------------------------------------------------------------------

data Command
  = Quit
  | Format String
  | TypeCheck String
  | Eval String
  | Load [String]
  deriving stock (Show, Read)

parseCmd :: String -> Either String Command
parseCmd (':' : input) = case break isSpace input of
  (map toLower -> cmd, dropWhile isSpace -> args)
    | cmd `isPrefixOf` "quit" -> Right Quit
    | cmd `isPrefixOf` "format" -> Right (Format args)
    | cmd `isPrefixOf` "type" -> Right (TypeCheck args)
    | cmd `isPrefixOf` "load" -> Right $ Load (words args)
    | otherwise -> Left ("unknown command :" ++ cmd)
parseCmd input = Right (Eval input)

-------------------------------------------------------------------------------

-- | Panini REPL settings with working autocomplete.
replSettings :: Maybe FilePath -> Settings Panini
replSettings histFile =
  Settings
    { complete = autocomplete,
      historyFile = histFile,
      autoAddHistory = True
    }

autocomplete :: CompletionFunc Panini
autocomplete = completeWord' Nothing isSpace $ \str -> do
  if null str
    then return []
    else return $ map simpleCompletion $ filter (str `isPrefixOf`) cmds
  where
    cmds = [":quit", ":format", ":type"]
