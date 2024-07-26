{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
module Panini.Frontend.Python (loadPythonSource) where

import Language.Python.Common qualified as Py
import Language.Python.Version3
import Panini.Error
import Panini.Frontend.Python.CFG as CFG
import Panini.Frontend.Python.DomTree
import Panini.Frontend.Python.Error as Py
import Panini.Frontend.Python.Transpiler
import Panini.Monad
import Panini.Pretty
--import Panini.Pretty.Graphviz
import Panini.Provenance
import Prelude
--import System.FilePath

loadPythonSource :: FilePath -> Pan ()
loadPythonSource fp = do
  src <- tryIO NoPV $ readFile fp
  case parseModule src fp of
    Left err -> do
      let err' = Py.ParserError err
      throwError $ PythonFrontendError err' (getPV err')
    Right (pyMod,_cmts) -> do
      logData $ pretty $ Py.prettyText pyMod
      --logData $ show pyMod
      case CFG.fromModule pyMod of
        Left err -> do
          throwError $ PythonFrontendError err (getPV err)
        Right cfg -> do
          logData $ pretty cfg
          --liftIO $ renderGraph ("trace_" <> takeBaseName fp <> ".svg") cfg
          let dom = domTree cfg
          --logData $ pretty dom
          --liftIO $ renderGraph ("trace_" <> takeBaseName fp <> ".dom.svg") dom

          case transpile dom of
            Left err2 -> throwError $ PythonFrontendError err2 (getPV err2)
            Right prog -> logData $ pretty prog
