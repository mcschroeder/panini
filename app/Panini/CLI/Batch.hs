module Panini.CLI.Batch where

import Control.Monad.Extra
import Data.Function
import Data.Maybe
import Data.Text.IO qualified as Text
import Options.Applicative
import Panini.CLI.Common
import Panini.CLI.Error
import Panini.CLI.Options
import Panini.Diagnostic
import Panini.Elab
import Panini.Elab.Environment
import Panini.Elab.Module
import Panini.Monad
import Panini.Pretty as PP
import Prelude
import System.Exit
import System.IO

-------------------------------------------------------------------------------

batchMain :: PanOptions -> IO ()
batchMain panOpts = do
  result <- withDiagnosticLogger panOpts $ \logger -> do
    let s0 = (panStateWithOptions panOpts) { diagnosticHandler = logger }
    -- TODO: add source lines for <stdin>
    runPan s0 $ do
      initialize
      moduleOrigin <- case panOpts.inputFile of
        Nothing -> Stdin <$> tryIO Text.getContents ?? AppIOError
        Just fp -> return $ File fp
      module_ <- loadModule panOpts moduleOrigin
      maybeSavePanFile panOpts module_
      elaborate module_ ?? ElabError
      vsep . map pretty . getSolvedTypes <$> gets environment

  case result of
    Left _ -> exitFailure
    Right (doc,panState1) -> do
      case panOpts.outputFile of
        Just fp -> withFile fp WriteMode $ \h -> hPutDoc panOpts h doc
        Nothing -> hPutDoc panOpts stdout $ doc <> "\n"
      case getTypeErrors panState1.environment of
        [] -> exitSuccess
        xs -> do
          hPutDoc panOpts stderr $ vsep (map prettyError xs) <> "\n"
          exitFailure
