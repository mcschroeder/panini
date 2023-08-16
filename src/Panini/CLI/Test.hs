{-# LANGUAGE RecordWildCards #-}
module Panini.CLI.Test (testMain) where

import Control.Exception
import Control.Monad.Extra
import Data.Either
import Data.Function
import Data.List qualified as List
import Data.Maybe
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Panini.CLI.Options
import Panini.Elab
import Panini.Modules
import Panini.Monad
import Panini.Parser
import Panini.Pretty
import Panini.SMT.Z3
import Prelude
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import Text.Printf

-------------------------------------------------------------------------------

testMain :: PanOptions -> IO ()
testMain globalOpts = assert globalOpts.testMode $ do
  whenJust globalOpts.outputFile $ \_ ->
      putStrLn $ "Warning: --output ignored in test mode"

  testFiles <- findTests $ fromMaybe "tests" globalOpts.inputFile
  results <- mapM runTest $ List.sort testFiles

  let total = length results
  let fails = total - sum (map fromEnum results)
  if fails == 0 then do
    putDocLn $ ann Success $ "All" <+> viaShow total <+> "tests passed"
    exitSuccess
  else do
    putDocLn $ ann Error $  viaShow fails <+> "out of" <+> 
                            viaShow total <+> "tests failed"
    exitFailure

 where
  testName :: String -> Doc
  testName inFile = pretty @String $ printf "%-40s " inFile

  runTest :: FilePath -> IO Bool
  runTest inFile = do
    putDoc $ testName inFile
    outFileExists <- doesFileExist (outFileFor inFile)
    errFileExists <- doesFileExist (errFileFor inFile)
    if outFileExists && errFileExists then do
      putDocLn $ ann Error "ambiguous golden files" <+>
        pretty [outFileFor inFile, errFileFor inFile]
      return False
    else
      execPan inFile >>= compareResult inFile

  -- TODO: read local options from inFile header comment
  execPan :: FilePath -> IO (Bool, Text)
  execPan inFile = do
    src <- Text.readFile inFile

    traceFile <- whenMaybe globalOpts.traceToFile (openLogFileFor inFile)
    when globalOpts.trace $ putDoc "\n"
    let panState0 = defaultState 
          { eventHandler = \ev -> do
              whenJust traceFile (putEventFile globalOpts ev)
              when globalOpts.trace (putEventStderr globalOpts ev)
              -- note how we don't log errors to stderr by default here 
          }

    result <- try @SomeException $ runPan panState0 $ do
      smtInit
      module_ <- liftIO $ getModule inFile
      prog <- parseSource (moduleLocation module_) src
      elaborate module_ prog
      getInferredTypes

    whenJust traceFile hClose
    when globalOpts.trace $ putDoc $ testName inFile

    let success = either (const False) isRight result
    let output = either viaShow (either pretty (vsep . map pretty)) result
    let actual = renderDoc (fileRenderOptions globalOpts) output
    return (success, actual)
  
  compareResult :: FilePath -> (Bool, Text) -> IO Bool
  compareResult inFile (success, actual) = do
    let goldenFile | success   = outFileFor inFile
                   | otherwise = errFileFor inFile
    goldenFileExists <- doesFileExist goldenFile
    if goldenFileExists then do
      expected <- Text.readFile goldenFile
      if actual /= expected then do
        putDocLn $ ann Error "FAIL" <\> diff goldenFile expected actual
        return False
      else do
        putDocLn $ ann Success "OK"
        return True
    else do
      let otherFile | success   = errFileFor inFile
                    | otherwise = outFileFor inFile
      otherFileExists <- doesFileExist otherFile
      if otherFileExists then do
        expected <- Text.readFile otherFile
        if actual /= expected then do
          putDocLn $ ann Error "FAIL" <\> diff otherFile expected actual
          return False
        else do
          putDocLn $ ann Error "FAIL" <+> "wrong exit code"
          return False
      else do
        withFile goldenFile WriteMode $ \h -> Text.hPutStr h actual      
        putDocLn $ 
          ann Message "created golden file" <\>
          ann Margin (divider symDivH (Just $ Right goldenFile)) <\>
          pretty actual <\>
          pretty (ann Margin $ divider symDivH Nothing) <> "\n"
        return True
  
  diff :: FilePath -> Text -> Text -> Doc
  diff expectedFile expected actual = 
    ann Margin (divider symDivH (Just $ Right expectedFile)) <\>
    pretty expected <\>
    ann Margin (divider symDivH (Just $ Right "Actual Output")) <\>
    pretty actual <\>    
    pretty (ann Margin $ divider symDivH Nothing) <> "\n"

  outFileFor, errFileFor :: FilePath -> FilePath
  outFileFor = flip replaceExtension ".out"
  errFileFor = flip replaceExtension ".err"

  putDoc :: Doc -> IO ()
  putDoc = putDocStdout globalOpts
  
  putDocLn :: Doc -> IO ()
  putDocLn d = putDoc $ d <> "\n"

-------------------------------------------------------------------------------

findTests :: FilePath -> IO [FilePath]
findTests inPath = doesDirectoryExist inPath >>= \case
    True -> findByExtension ".pan" inPath
    False -> doesFileExist inPath >>= \case
      True -> return [inPath]
      False -> return []

findByExtension :: String -> FilePath -> IO [FilePath]
findByExtension ext dir = do
  entries <- listDirectory dir    
  concatForM entries $ \e -> do
    let path = dir </> e
    doesDirectoryExist path >>= \case
      True -> findByExtension ext path
      False 
        | ext `isExtensionOf` path -> return [path]
        | otherwise                -> return []
