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
format input = do
  e <- parseExpr input
  whenJust e printExpr

unknown :: Text -> InputT IO ()
unknown cmd = do
  outputStrLn $ "unknown command :" ++ Text.unpack cmd

-------------------------------------------------------------------------------

parseExpr :: Text -> InputT IO (Maybe Expr)
parseExpr input = case parse (expr <* eof) "<repl>" input of
  Left bundle -> do
    outputStrLn (errorBundlePretty bundle)
    return Nothing
  Right x -> return $ Just x

printExpr :: Expr -> InputT IO ()
printExpr = liftIO . prettyPut

