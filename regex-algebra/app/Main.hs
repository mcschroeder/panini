module Main where

import Data.Void
import Options
import Options.Applicative (execParser)
import Prelude
import Regex
import Regex.POSIX.ERE as ERE
import System.Exit (exitFailure)
import System.IO
import Text.Megaparsec
import Text.Read
import FuzzingBook as FB

-------------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- execParser optionsParser
  
  hSetBuffering stdin LineBuffering

  (inputPath, inputString) <- case opts.inputFile of
    Nothing   -> ("<stdin>",) <$> getLine
    Just "-"  -> ("<stdin>",) <$> getLine
    Just file -> (     file,) <$> readFile' file
  
  inputRegex <- case opts.inputFormat of
    Just POSIX -> fromPOSIX inputPath inputString
    Nothing    -> fromPOSIX inputPath inputString
    _          -> error "unsupported input format"
  
  let outputRegex | opts.simplify = Regex.simplify inputRegex
                  | otherwise     = inputRegex

  outputString <- case opts.outputFormat of
    Nothing           -> pure $ toPOSIX outputRegex
    Just POSIX        -> pure $ toPOSIX outputRegex
    Just FuzzingBook  -> pure $ toFuzzingBook outputRegex

  case opts.outputFile of
    Nothing   -> putStr outputString
    Just "-"  -> putStr outputString
    Just file -> writeFile file outputString

-------------------------------------------------------------------------------

fromPOSIX :: FilePath -> String -> IO Regex
fromPOSIX fp str = case parse (ere @Void) fp str of
  Left err -> putStr (errorBundlePretty err) >> exitFailure
  Right ere -> return $ ERE.toRegex ere 

-------------------------------------------------------------------------------

toPOSIX :: Regex -> String
toPOSIX r = case ERE.fromRegex r of
  Just ere -> printERE ere
  Nothing  -> error "cannot convert regex to POSIX"

toFuzzingBook :: Regex -> String
toFuzzingBook = printGrammar . FB.fromRegex
