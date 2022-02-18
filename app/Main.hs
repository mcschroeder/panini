{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Language.Panini.Parser
import Language.Panini.Printer
import Language.Panini.Syntax
import System.Environment (getArgs)
import System.IO
import Text.Megaparsec

main :: IO ()
main = undefined

-- main :: IO ()
-- main = do
--   putStrLn "Panini"
--   let file = "examples/fac.pan"
--   src <- Text.readFile file
--   case parse (expr <* eof) file src of
--     Left bundle -> putStr (errorBundlePretty bundle)
--     Right e -> printExpr e

-- parserREPL :: IO ()
-- parserREPL =
--   forever $ parseTest (expr <* eof) =<< getLines ""
--   where
--     getLines s1 = do
--       s2 <- Text.getLine
--       if s2 == "" then return s1 else getLines (s1 <> s2)
