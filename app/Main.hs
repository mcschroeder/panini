{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Char (isSpace)
import Data.List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO (getLine)
import Data.Text.IO qualified as Text
import System.Console.ANSI
import System.Console.Haskeline
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO

import Language.Panini.Parser
import Language.Panini.Printer
import Language.Panini.Syntax

-------------------------------------------------------------------------------

main :: IO ()
main = do
  configDir <- getXdgDirectory XdgConfig "panini"
  createDirectoryIfMissing True configDir
  let settings = Settings 
        { complete = autocomplete
        , historyFile = Just (configDir </> "pan_history")
        , autoAddHistory = True
        }
  runInputT settings repl

-------------------------------------------------------------------------------

commands :: [String]
commands = [":quit", ":format"]

autocomplete :: CompletionFunc IO
autocomplete = completeWord' Nothing isSpace $ \str -> do
  if null str
    then return []
    else return $ map simpleCompletion $ filter (str `isPrefixOf`) commands

data Command
  = Quit
  | Format Text
  | UnknownCommand Text
  deriving (Show, Read)

parseCmd :: String -> Command
parseCmd str = case Text.break isSpace $ Text.pack str of
  (Text.toLower -> cmd, Text.stripStart -> input)
    | Text.isPrefixOf cmd "quit" -> Quit
    | Text.isPrefixOf cmd "format" -> Format input
    | otherwise -> UnknownCommand cmd

repl :: InputT IO ()
repl = getInputLine "Panini> " >>= \case
  Just (':':input) -> case parseCmd input of
    Quit               -> return ()
    Format input       -> format input      >> repl
    UnknownCommand cmd -> unknown cmd       >> repl    
  Just input           -> outputStrLn input >> repl
  Nothing              -> return ()

-------------------------------------------------------------------------------

format :: Text -> InputT IO ()
format input = 
  case parseExpr "<repl>" input of
    Left err -> outputStrLn err
    Right ex -> outputExpr ex

unknown :: Text -> InputT IO ()
unknown cmd = do
  outputStrLn $ "unknown command :" ++ Text.unpack cmd

-------------------------------------------------------------------------------

outputExpr :: Expr -> InputT IO ()
outputExpr e = do
  opts <- getPrintOptions
  let t = printExpr opts e
  liftIO $ Text.putStrLn t

getPrintOptions :: InputT IO PrintOptions
getPrintOptions = liftIO $ do
  ansiColors <- hSupportsANSIColor stdout
  fixedWidth <- fmap snd <$> getTerminalSize
  return PrintOptions { unicodeSymbols = True, ansiColors, fixedWidth }    
