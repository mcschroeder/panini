module Options where

import Data.Char
import Options.Applicative
import Prelude

-------------------------------------------------------------------------------

data Format = POSIX | FuzzingBook
  deriving stock (Show)

fmt :: ReadM Format
fmt = str >>= \s -> case map toLower s of
    "posix"       -> return POSIX
    "fuzzingbook" -> return FuzzingBook
    _             -> readerError "invalid format"

data Options = Options
  { inputFile     :: Maybe FilePath
  , outputFile    :: Maybe FilePath
  , inputFormat   :: Maybe Format
  , outputFormat  :: Maybe Format
  , simplify      :: Bool
  }
  deriving stock (Show)

optionsParser :: ParserInfo Options
optionsParser = info
  (options <**> helper <**> simpleVersioner "0.1")
  (fullDesc <> progDesc "Regular Expression Toolkit")
 where
  options = Options
    <$> (optional $ strArgument $ metavar "INPUT" <> 
          help "Input file (default: stdin)")
    <*> (optional $ strOption $ long "output" <> short 'o' <> metavar "FILE" <>
          help "Write output to FILE instead of stdout")
    <*> (optional $ option fmt $ long "from" <> short 'f' <> metavar "FORMAT" <>
          help "Specify input format")
    <*> (optional $ option fmt $ long "to" <> short 't' <> metavar "FORMAT" <>
          help "Specify output format")
    <*> (switch $ long "simplify" <>
          help "Simplify regular expression")
