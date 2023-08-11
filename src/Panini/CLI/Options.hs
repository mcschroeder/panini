module Panini.CLI.Options where

import Control.Monad.Extra
import Data.List qualified as List
import Options.Applicative
import Panini.Pretty.Printer as PP
import Prelude
import Prettyprinter.Util (reflow)
import System.Console.ANSI

-------------------------------------------------------------------------------

data PanOptions = PanOptions
  { inputFile :: Maybe FilePath
  , noInput :: Bool
  , outputFile :: Maybe FilePath
  , outputGrammars :: Bool
  , trace :: Bool
  , traceToFile :: Bool
  , color :: Bool
  , unicode :: Bool
  , testMode :: Bool
  }
  deriving stock (Show, Read)

opts :: ParserInfo PanOptions
opts = info 
  (panOptions <**> helper <**> simpleVersioner "v0.1") 
  (fullDesc <> progDesc "Grammar Inference for Ad Hoc Parsers" <> 
    (footerDoc $ Just $ paragraphs
      [ "If no INPUT file is given and stdin is an interactive terminal,\
        \ Panini launches into a REPL. If stdin is not interactive, or the\
        \ --no-input flag is passed, input is read from stdin."
      , "If the --test flag is passed, Panini runs in test mode: if INPUT is\
        \ a file, any output is compared against a correspondingly named .out\
        \ file; if INPUT is a directory, all files matching INPUT/*.in will\
        \ be processed and their outputs compared against the corresponding\
        \ .out files; if no INPUT is given, it is per default assumed to be\
        \ a directory named \"tests\". If any test input file has no matching\
        \ .out file yet, one will be automatically created from the current\
        \ output. Both --trace and --trace-file are available in test mode."
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
            long "grammars" <>
            short 'g' <>
            help "Output only inferred grammars"
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

-------------------------------------------------------------------------------

getTermRenderOptions :: PanOptions -> IO RenderOptions
getTermRenderOptions panOpts = do
  termWidth <- fmap snd <$> getTerminalSize
  return RenderOptions
    { styling = pureIf panOpts.color defaultStyling
    , PP.unicode = panOpts.unicode
    , fixedWidth = termWidth
    }

fileRenderOptions :: PanOptions -> RenderOptions
fileRenderOptions panOpts = RenderOptions 
  { styling = Nothing
  , PP.unicode = panOpts.unicode
  , fixedWidth = Nothing
  }
