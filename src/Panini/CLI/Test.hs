{-# LANGUAGE RecordWildCards #-}
module Panini.CLI.Test (testMain) where

import Control.Exception
import Control.Monad.Extra
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

-------------------------------------------------------------------------------

testMain :: PanOptions -> IO ()
testMain globalOpts = assert globalOpts.testMode $ do
  whenJust globalOpts.outputFile $ \_ ->
      putStrLn $ "Warning: --output ignored in test mode"

  testFiles <- findTestPairs $ fromMaybe "tests" globalOpts.inputFile
  results <- mapM runTest $ List.sort testFiles
  let total = length results
  let fails = total - sum (map fromEnum results)
  if fails == 0
    then do
      putDocLn $ ann Success $ "All" <+> viaShow total <+> "tests passed"
      exitSuccess
    else do
      putDocLn $ ann Error $ 
        viaShow fails <+> "out of" <+> viaShow total <+> "tests failed"
      exitFailure
  
 where
  -- TODO: pass options in infile header comments
  runTest :: (FilePath, FilePath) -> IO Bool
  runTest (inFile, outFile) = do
    if globalOpts.trace
      then putDocLn $ pretty inFile <+> "..."
      else putDoc   $ pretty inFile <+> "... "

    src <- Text.readFile inFile

    traceFile <- whenMaybe globalOpts.traceToFile (openLogFileFor inFile)  

    let eventHandler ev = do
          whenJust traceFile (putEventFile globalOpts ev)
          when globalOpts.trace (putEventStderr globalOpts ev)
          -- note how we don't log errors to stderr by default here

    let panState0 = defaultState { eventHandler }

    result <- try @SomeException $ runPan panState0 $ do
      smtInit
      module_ <- liftIO $ getModule inFile
      prog <- parseSource (moduleLocation module_) src
      elaborate module_ prog
      getInferredTypes

    whenJust traceFile hClose

    when globalOpts.trace $
      putDoc $ pretty inFile <+> "... "

    let output = either viaShow (either pretty (vsep . map pretty)) result
    let actual = renderDoc (fileRenderOptions globalOpts) output
    doesFileExist outFile >>= \case
      False -> do
        withFile outFile WriteMode $ \h -> Text.hPutStr h actual      
        putDocLn $ "output file did not exist; created"
        return True
      True -> do
        expected <- Text.readFile outFile
        if actual /= expected
          then do
            putDocLn $ ann Error "FAIL" <\> diff expected actual
            return False
          else do
            putDocLn $ ann Success "OK"
            return True

  diff :: Text -> Text -> Doc
  diff expected actual = 
    ann Margin (divider symDivH (Just $ Left "Expected")) <\>
    pretty expected <\>
    ann Margin (divider symDivH (Just $ Left "Actual")) <\>
    pretty actual <\>
    pretty (ann Margin $ divider symDivH Nothing) <> "\n"

  putDoc :: Doc -> IO ()
  putDoc = putDocStdout globalOpts
  
  putDocLn :: Doc -> IO ()
  putDocLn d = putDoc $ d <> "\n"

-------------------------------------------------------------------------------

findTestPairs :: FilePath -> IO [(FilePath,FilePath)]
findTestPairs inPath = doesDirectoryExist inPath >>= \case
    True -> map mkPair <$> findByExtension ".in" inPath
    False -> doesFileExist inPath >>= \case
      True -> return [mkPair inPath]
      False -> return []
 where
  mkPair inFile = (inFile, inFile -<.> ".out")

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
