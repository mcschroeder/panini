module Panini.REPL
  ( Panini,
    PaniniState (..),
    paniniInitState,
    repl,
    replSettings,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Char (isSpace, toLower)
import Data.List (isPrefixOf)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Panini.Core.Checker
import Panini.Core.Parser
import Panini.Core.Printer
import Panini.Core.Syntax
import System.Console.ANSI
import System.Console.Haskeline
import System.IO
import Prelude
import Control.Monad
import Control.Monad.Trans.Class

-------------------------------------------------------------------------------

-- | Panini REPL monad.
type Panini = StateT PaniniState IO

-- | Global state for Panini REPL.
data PaniniState = PaniniState
  {}

paniniInitState :: PaniniState
paniniInitState = PaniniState {}

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
        Eval args -> outputStrLn args >> repl -- TODO
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
      let g0 = emptyCtx
      case synth g0 e of
        Left err2 -> do
          outputStrLn ""
          outputStrLn $ Text.unpack $ printTypeError opts "<repl>" err2
        Right (c, t) -> do
          outputStrLn $ Text.unpack $ printCon opts c
          outputStrLn ""
          outputStrLn $ Text.unpack $ printType opts t

elabProg :: Prog -> Panini ()
elabProg = undefined

loadModules :: [String] -> InputT Panini ()
loadModules ms = forM_ ms $ \m -> do
  src <- liftIO $ Text.readFile m
  case parseProg m src of
    Left err1 -> outputStrLn err1
    Right prog -> do
      --lift $ elabProg prog
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
