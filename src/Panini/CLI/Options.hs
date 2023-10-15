module Panini.CLI.Options where

import Control.Monad.Extra
import Data.List qualified as List
import Data.Text.IO qualified as Text
import Options.Applicative
import Panini.Events
import Panini.Pretty as PP
import Panini.Version
import Prelude
import Prettyprinter.Util (reflow)
import System.FilePath
import System.IO

-- TODO: validate smtTimeout option (>= 0)

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
  , createGoldenFiles :: Bool
  , termWidth :: Maybe Int
  , smtTimeout :: Int
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
        \ a file, any output is compared against a correspondingly named\
        \ \"golden file\" with an extension of either .out or .err, depending\
        \ on the expected outcome; if INPUT is a directory, all files matching\
        \ INPUT/*.pan will be processed and their outputs compared against the\
        \ corresponding golden files; if no INPUT is given, it is per default\
        \ assumed to be a directory named \"tests\". If any test input file\
        \ has no matching golden file yet and the --create-golden-files flag\
        \ is passed, a corresponding golden file will be automatically created\
        \ from the current output. Both --trace and --trace-file are available\
        \ in test mode."
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
            help "SMT solver timeout (default: 10 seconds)" <>
            value 10
          )

-------------------------------------------------------------------------------

openLogFileFor :: FilePath -> IO Handle
openLogFileFor f = do
  h <- openFile (f -<.> "log") WriteMode
  hSetBuffering h NoBuffering
  return h

putEventFile :: PanOptions -> Event -> Handle -> IO ()
putEventFile o e = putDocFile o $ prettyEvent e <> "\n"

putEventStderr :: PanOptions -> Event -> IO ()
putEventStderr o e = putDocStderr o $ prettyEvent e <> "\n"

-------------------------------------------------------------------------------

putDocFile :: PanOptions -> Doc -> Handle -> IO ()
putDocFile o d h = hPutDoc (fileRenderOptions o) d h

putDocStderr :: PanOptions -> Doc -> IO ()
putDocStderr o d = hPutDoc (termRenderOptions o) d stderr

putDocStdout :: PanOptions -> Doc -> IO ()
putDocStdout o d = hPutDoc (termRenderOptions o) d stdout

hPutDoc :: RenderOptions -> Doc -> Handle -> IO ()
hPutDoc o d h = Text.hPutStr h $ renderDoc o d

-------------------------------------------------------------------------------

fileRenderOptions :: PanOptions -> RenderOptions
fileRenderOptions o = RenderOptions 
  { styling = Nothing
  , PP.unicode = o.unicode
  , fixedWidth = Nothing 
  }

termRenderOptions :: PanOptions -> RenderOptions
termRenderOptions o = RenderOptions
  { styling = pureIf o.color defaultStyling
  , PP.unicode = o.unicode
  , fixedWidth = o.termWidth
  }
