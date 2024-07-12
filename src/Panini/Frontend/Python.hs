{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
module Panini.Frontend.Python (loadPythonSource) where

import Language.Python.Common qualified as Py
import Language.Python.Version3
import Panini.Frontend.Python.CFG as CFG
import Panini.Frontend.Python.DomTree
import Panini.Frontend.Python.SSA
import Panini.Frontend.Python.Pretty
import Panini.Monad
import Panini.Pretty
import Panini.Pretty.Graphviz
import Panini.Provenance
import Prelude
import System.FilePath
import Panini.Error
import Data.IntMap.Strict qualified as IntMap
import Control.Monad

loadPythonSource :: FilePath -> Pan ()
loadPythonSource fp = do
  src <- tryIO NoPV $ readFile fp
  case parseModule src fp of
    Left _err -> undefined --throwError $ ParserError undefined undefined
    Right (pyMod,_cmts) -> do
      logData $ pretty $ Py.prettyText pyMod
      --logData $ show pyMod
      case CFG.fromModule pyMod of
        Left err -> do
          throwError $ err2err err
        Right cfg -> do
          logData $ pretty cfg
          liftIO $ renderGraph ("trace_" <> takeBaseName fp <> ".svg") cfg

          let d = domTree cfg
          logData $ show d
          liftIO $ renderGraph ("trace_" <> takeBaseName fp <> ".dom.svg") d
          let phi = phiFuncs d cfg
          logData $ show phi
          
          forM_ (IntMap.elems cfg.nodeMap) $ \case
            FunDef{..} -> do
              let d' = domTree _body
              logData $ show d'
              liftIO $ renderGraph ("trace_" <> takeBaseName fp <> "_" <> showPretty _name <> ".dom.svg") d'
              let phi' = phiFuncs d' _body
              logData $ show phi'
              
            _ -> return ()



err2err :: CFG.Error -> Panini.Error.Error
err2err (Unsupported stmt) = 
  IOError "unsupported statement" (pySpanToPV $ keywordSpan stmt)
