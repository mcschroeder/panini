{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Char (isSpace)
import Data.List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO (getLine)
import Data.Text.IO qualified as Text
import Language.Panini.Parser
import Language.Panini.Printer
import Language.Panini.Syntax
import System.Console.Haskeline
import System.Environment (getArgs)
import System.IO
import Text.Megaparsec
import Text.Megaparsec (errorBundlePretty)
import System.Directory
import System.FilePath

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
      
autocomplete :: CompletionFunc IO
autocomplete = completeWord' Nothing isSpace $ \str -> do
  if null str
    then return []
    else return $ map simpleCompletion $ filter (str `isPrefixOf`) commands

commands :: [String]
commands = [":quit", ":format"]

repl :: InputT IO ()
repl = do
  input0 <- getInputLine "Panini> "
  case input0 of
    Nothing -> return ()
    Just (':':input) -> case parseCmd input of
      Quit -> return ()
      Format input -> do
        case parse (expr <* eof) "<repl>" input of
          Left bundle -> outputStrLn (errorBundlePretty bundle)
          Right x -> liftIO $ prettyPut x
        repl
      UnknownCommand cmd -> do
        outputStrLn $ "unknown command :" ++ Text.unpack cmd
        repl
    Just input -> do
      outputStrLn input
      repl

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
