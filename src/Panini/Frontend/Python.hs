{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
module Panini.Frontend.Python where

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
import Panini.Frontend.Python.Typing.Infer
import Panini.Frontend.Python.Typing.Pretty ()
import Panini.Frontend.Inliner
import Panini.Modules
import Data.Text (Text)
import Data.Text qualified as Text
import Panini.Syntax

loadModulePython :: Text -> FilePath -> Pan (Module, Program)
loadModulePython src fp = do
  case parseModule (Text.unpack src) fp of
    Left err -> do
      let err' = Py.ParserError err
      throwError $ PythonFrontendError err' (getPV err')
    Right (pyMod,_cmts) -> do
      logData $ pretty pyMod      
      case infer pyMod of
        Left err -> do
          let err' = Py.TypeError err
          throwError $ PythonFrontendError err' (getPV err')
        Right pyModTy -> do
          logData $ pretty pyModTy                    
          case CFG.fromModule pyModTy of
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
                Right prog -> do
                  logData $ pretty prog
                  let prog2 = inlineProgram prog
                  logData $ pretty prog2

                  module_ <- liftIO $ getModule fp
                  return (module_, prog2)
