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
import Panini.CLI.Common
import Panini.CLI.Error
import Panini.CLI.Options
import Panini.Diagnostic
import Panini.Elab
import Panini.Elab.Environment
import Panini.Elab.Module
import Panini.Monad
import Panini.Pretty
import Prelude
import System.Directory
import System.Exit
import System.FilePath
import System.FilePattern.Directory
import System.IO
import System.Time.Extra
import Text.Printf

-------------------------------------------------------------------------------

testMain :: PanOptions -> IO ()
testMain globalOpts = assert globalOpts.testMode $ do
  whenJust globalOpts.outputFile $ \_ ->
      putStrLn $ "Warning: --output ignored in test mode"
  
  hSetBuffering stdout NoBuffering

  testFiles <- findTests $ fromMaybe "tests" globalOpts.inputFile
  results <- mapM (runTest globalOpts) $ List.sort testFiles

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
  testName inFile = pretty @String $ printf "%-50s " inFile

  runTest :: PanOptions -> FilePath -> IO Bool
  runTest panOpts inFile = do
    putDoc $ testName inFile
    execPan panOpts inFile >>= compareResult panOpts inFile

  -- TODO: read local options from inFile header comment
  execPan :: PanOptions -> FilePath -> IO (Seconds, Text)
  execPan panOpts inFile = do
    when globalOpts.trace $ putDoc "\n"

    (time, result) <- withDiagnosticLogger globalOpts $ \logger -> do
      let s0 = (panStateWithOptions globalOpts) { diagnosticHandler = logger }
      duration $ try @SomeException $ runPan s0 $ do
        initialize
        module_ <- loadModule panOpts (File inFile)
        maybeSavePanFile panOpts module_
        elaborate module_ ?? ElabError
        (es,ts) <- liftM2 (,) getTypeErrors getSolvedTypes <$> gets environment
        return $ vsep $ (map prettyError es) ++ (map pretty ts)

    when globalOpts.trace $ putDoc $ testName inFile

    case result of
      Left e | Just UserInterrupt <- asyncExceptionFromException e -> throw e
      _ -> return ()

    let output = either viaShow (either prettyError fst) result
    let actual = renderDoc testOutputRenderOptions output
    return (time, actual)
  
  testOutputRenderOptions :: RenderOptions
  testOutputRenderOptions = RenderOptions 
    { styling = Nothing
    , unicode = globalOpts.unicode
    , fixedWidth = Nothing 
    }
  
  compareResult :: PanOptions -> FilePath -> (Seconds, Text) -> IO Bool
  compareResult panOpts inFile (time, actual) = do
    let durationStr
          | panOpts.milliseconds = show @Int $ round $ time * 1000
          | otherwise = showDuration time
    let prettyTime = ann Margin $ parens $ pretty durationStr
    let goldenFile = outFileFor inFile
    goldenFileExists <- doesFileExist goldenFile
    if goldenFileExists then do
      expected <- Text.readFile goldenFile
      if actual /= expected then do
        putDocLn $ 
          ann Error "FAIL" <+> prettyTime <\> 
          diff goldenFile expected actual
        return False
      else do
        putDocLn $ ann Success "OK" <+> prettyTime
        return True
    else if globalOpts.createGoldenFiles then do        
      withFile goldenFile WriteMode $ \h -> Text.hPutStr h actual      
      putDocLn $ 
        ann Message "created golden file" <+> prettyTime <\>
        ann Margin (divider symDivH (Just $ Right goldenFile)) <\>
        pretty actual <\>
        pretty (ann Margin $ divider symDivH Nothing) <> "\n"
      return True
    else do
      putDocLn $ 
        ann Message "missing golden file" <+> prettyTime <\>
        ann Margin (divider symDivH (Just $ Right "Output")) <\>
        pretty actual <\>
        pretty (ann Margin $ divider symDivH Nothing) <> "\n"
      return True
  
  diff :: FilePath -> Text -> Text -> Doc
  diff expectedFile expected actual = 
    ann Margin (divider symDivH (Just $ Right "Actual Output")) <\>
    pretty actual <\>    
    ann Margin (divider symDivH (Just $ Right expectedFile)) <\>
    pretty expected <\>
    pretty (ann Margin $ divider symDivH Nothing) <> "\n"

  outFileFor :: FilePath -> FilePath
  outFileFor = flip addExtension ".out"

  putDoc :: Doc -> IO ()
  putDoc = hPutDoc globalOpts stdout
  
  putDocLn :: Doc -> IO ()
  putDocLn d = putDoc $ d <> "\n"

-------------------------------------------------------------------------------

findTests :: FilePattern -> IO [FilePath]
findTests pat
  | hasExtension pat = go [pat]
  | otherwise        = go [ pat' ++ glob ++ ext 
                          | let pat' = dropTrailingPathSeparator pat
                          , glob <- ["*/**/*","*"]
                          , ext <- testFileExtensions
                          ]
 where
  go pats = do
    cwd <- getCurrentDirectory
    getDirectoryFiles cwd pats

testFileExtensions :: [String]
testFileExtensions = [".pan",".py"]
