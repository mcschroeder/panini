module Panini.CLI.Options where

import Data.List qualified as List
import Options.Applicative
import Panini.Version
import Prelude
import Prettyprinter.Util (reflow)

-- TODO: validate smtTimeout option (>= 0)

-------------------------------------------------------------------------------

data PanOptions = PanOptions
  { inputFile :: Maybe String
  , noInput :: Bool
  , outputFile :: Maybe FilePath
  , trace :: Bool
  , traceToFile :: Bool
  , color :: Bool
  , unicode :: Bool
  , testMode :: Bool
  , createGoldenFiles :: Bool
  , termWidth :: Maybe Int
  , smtTimeout :: Int
  , regexTimeout :: Double
  , pythonInput :: Bool
  , debugTraceFrontendGraph :: Bool
  , savePanFile :: Bool
  , milliseconds :: Bool
  }
  deriving stock (Show, Read)

opts :: ParserInfo PanOptions
opts = info 
  (panOptions <**> helper <**> simpleVersioner version)
  (fullDesc <> progDesc "Grammar Inference for Ad Hoc Parsers" <> 
    (footerDoc $ Just $ paragraphs
      [ "If no INPUT file is given and stdin is an interactive terminal,\
        \ Panini launches into a REPL. If stdin is not interactive, or the\
        \ --no-input flag is passed, input is read from stdin."
      , "If the --test flag is passed, Panini runs in test mode: if INPUT is\
        \ a file, any output is compared against a corresponding \"golden\
        \ file\" named INPUT.out; if INPUT is a directory or file pattern, all\
        \ files matching INPUT/**/*.{pan,py} will be processed and their outputs\
        \ compared against the corresponding golden files; if no INPUT is\
        \ given, it is per default assumed to be a directory named \"tests\".\
        \ If any test input file has no matching golden file yet and the\
        \ --create-golden-files flag is passed, a corresponding golden file\
        \ will be automatically created from the current output. Both --trace\
        \ and --trace-file are available in test mode."
      ])
  )
  where
    paragraphs = mconcat . List.intersperse "\n\n" . map reflow
    panOptions = PanOptions
      <$> (optional $ strArgument $ 
            metavar "INPUT" <> 
            help "Input file (default: stdin)"
          )
      <*> (switch $
            long "no-input" <>
            help "Don't do anything interactive (disables REPL)"
          )
      <*> (optional $ strOption $ 
            long "output" <> 
            short 'o' <> 
            metavar "FILE" <> 
            help "Write output to FILE (default: stdout)"
          )
      <*> (switch $ 
            long "trace" <> 
            help "Show a detailed inference trace on stderr"
          )
      <*> (switch $
            long "trace-file" <>
            help "Write a detailed inference trace to a file"
          )
      <*> (flag True False $ 
            long "no-color" <> 
            help "Disable color output to terminal"
          )
      <*> (flag True False $ 
            long "no-unicode" <> 
            help "Disable Unicode output to terminal and files"
          )
      <*> (switch $
            long "test" <>
            help "Run tests"
          )
      <*> (switch $
            long "create-golden-files" <>
            help "Automatically create missing golden files"
          )
      <*> (optional $ option auto $
            long "term-width" <>
            hidden
          )
      <*> (option auto $
            long "smt-timeout" <>
            metavar "SECONDS" <>
            help "SMT solver timeout (default: 1 second)" <>
            value 1
          )
      <*> (option auto $
            long "regex-timeout" <>
            metavar "SECONDS" <>
            help "Regex simplifier timeout (default: 5 seconds)" <>
            value 5
          )
      <*> (switch $
            long "python" <>
            help "Force input to be read as Python"
          )
      <*> (switch $
            long "debug-trace-frontend-graph" <>
            hidden
          )
      <*> (switch $
            long "save-pan" <>
            help "Save the transpiled source as a .pan file"
          )
      <*> (switch $
            long "milliseconds" <>
            help "Show timings in milliseconds"
          )
