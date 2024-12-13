module Panini.Frontend.Python (loadModulePython) where

import Control.Monad.Extra
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Python.Version3
import Panini.Frontend.Inliner
import Panini.Frontend.Python.CFG qualified as CFG
import Panini.Frontend.Python.DomTree
import Panini.Frontend.Python.Error qualified as Py
import Panini.Frontend.Python.Provenance
import Panini.Frontend.Python.Transpiler
import Panini.Frontend.Python.Typing.Infer
import Panini.Frontend.Python.Typing.Pretty ()
import Panini.Modules
import Panini.Monad
import Panini.Pretty.Graphviz
import Panini.Syntax
import Panini.Pretty
import Prelude
import Panini.CLI.Error

loadModulePython :: Text -> FilePath -> Pan AppError (Module, Program)
loadModulePython src fp = do
  let src'   = Text.unpack src
  (pyMod,_) <- parseModule src' fp     ? paErr §§ "Parse Python source"
  let pyModPV = convertProvenance pyMod
  pyModTy   <- infer pyModPV           ? tyErr §§ "Infer Python types"
  cfg       <- CFG.fromModule pyModTy  ? pyErr §§ "Construct Python CFG"
  let dom    = domTree cfg
  debugTraceGraph dom
  prog      <- transpile dom           ? pyErr §§ "Transpile Python to Panini"
  prog2     <- inlineProgram prog               § "Inline bindings"
  module_   <- liftIO $ getModule fp
  return     $ (module_, prog2)
 where
  paErr = \e -> pyErr $ Py.ParserError e (getParseErrorPV e)
  tyErr = pyErr . Py.TypeError
  pyErr = PythonError

  debugTraceGraph dom = whenM (gets debugTraceFrontendGraph) $ do
    let graphFile = fp <> ".dom.svg"
    logMessage $ "Render dominator tree / CFG to" <+> pretty graphFile
    liftIO $ renderGraph graphFile dom
