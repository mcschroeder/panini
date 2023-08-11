{-# LANGUAGE RecordWildCards #-}
module Panini.CLI.Test (testMain) where

import Control.Exception
import Control.Monad.Extra
import Control.Monad.Trans.State.Strict
import Data.Function
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe
import Data.Text.IO qualified as Text
import Panini.CLI.Options
import Panini.Elab
import Panini.Environment
import Panini.Modules
import Panini.Monad
import Panini.Parser
import Panini.Pretty.Printer
import Panini.Provenance
import Panini.SMT.Z3
import Prelude
import System.Directory
import System.Exit
import System.FilePath
import System.IO

-------------------------------------------------------------------------------

testMain :: PanOptions -> IO ()
testMain panOpts = assert panOpts.testMode $ do
  whenJust panOpts.outputFile $ \_ ->
      putStrLn $ "Warning: --output ignored in test mode"

  testFiles <- findTestPairs $ fromMaybe "tests" panOpts.inputFile
  results <- mapM (runTest panOpts) testFiles
  if and results
    then exitSuccess
    else exitFailure

-- TODO: write diff to console
-- TODO: pass options in infile header comments
runTest :: PanOptions -> (FilePath, FilePath) -> IO Bool
runTest globalOpts (inFile, outFile) = do
  putStr $ inFile ++ " ... "
  src <- Text.readFile inFile

  traceFile <- whenMaybe globalOpts.traceToFile (openLogFileFor inFile)  

  let eventHandler ev = do
        whenJust traceFile (putEventFile globalOpts ev)
        when globalOpts.trace (putEventStderr globalOpts ev)
        -- note how we don't log errors to stderr by default here

  let panState0 = defaultState { eventHandler }

  result <- runPan panState0 $ do
    smtInit
    module_ <- liftIO $ getModule inFile
    prog <- parseSource (moduleLocation module_) src
    elaborate module_ prog
    getInferredTypes

  whenJust traceFile hClose
  
  let output = either pretty (vsep . map pretty) result
  let renderOpts = fileRenderOptions globalOpts
  let actual = renderDoc renderOpts output
  doesFileExist outFile >>= \case
    False -> do
      withFile outFile WriteMode $ \h -> Text.hPutStr h actual
      putStrLn "output file did not exist; created"
      return True
    True -> do
      expected <- Text.readFile outFile
      if actual /= expected
        then do
          putStrLn "FAIL"
          return False
        else do
          putStrLn "OK"
          return False

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
