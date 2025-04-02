module Main where

import Data.Maybe
import Options.Applicative
import Panini.CLI.Batch
import Panini.CLI.Options
import Panini.CLI.REPL
import Panini.CLI.Test
import Prelude
import System.Console.Terminal.Size
import System.Environment
import System.IO

main :: IO ()
main = do
  panOpts0 <- execParser opts  
  noColor <- maybe False (not . null) <$> lookupEnv "NO_COLOR"
  termWidth <- fmap width <$> size
  let panOpts = panOpts0 
        { color = panOpts0.color && not noColor
        , termWidth = panOpts0.termWidth <|> termWidth
        }
  
  if panOpts.testMode 
    then testMain panOpts
    else do
      isTerm <- hIsTerminalDevice stdin
      if isNothing panOpts.inputFile && isTerm && not panOpts.noInput
        then replMain panOpts
        else batchMain panOpts
