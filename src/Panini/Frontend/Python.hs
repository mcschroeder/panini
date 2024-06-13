{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
module Panini.Frontend.Python (loadPythonSource) where

import Language.Python.Common.Pretty qualified as Py
import Language.Python.Common.PrettyAST ()
import Language.Python.Version3
import Panini.Frontend.Python.CFG as CFG
import Panini.Monad
import Panini.Pretty
import Panini.Provenance
import Prelude

loadPythonSource :: FilePath -> Pan ()
loadPythonSource fp = do
  src <- tryIO NoPV $ readFile fp
  case parseModule src fp of
    Left _err -> undefined --throwError $ ParserError undefined undefined
    Right (pyMod,_cmts) -> do
      logData $ pretty $ Py.prettyText pyMod
      --logData $ show pyMod
      let cfg = CFG.fromModule pyMod
      logData $ pretty cfg
