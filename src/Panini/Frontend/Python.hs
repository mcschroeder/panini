module Panini.Frontend.Python (loadModulePython) where

import Data.Text (Text)
import Data.Text qualified as Text
import Language.Python.Version3
import Panini.Error
import Panini.Frontend.Inliner
import Panini.Frontend.Python.CFG qualified as CFG
import Panini.Frontend.Python.DomTree
import Panini.Frontend.Python.Error qualified as Py
import Panini.Frontend.Python.Transpiler
import Panini.Frontend.Python.Typing.Infer
import Panini.Frontend.Python.Typing.Pretty ()
import Panini.Modules
import Panini.Monad
import Panini.Syntax
import Prelude

loadModulePython :: Text -> FilePath -> Pan (Module, Program)
loadModulePython src fp = do
  let src'   = Text.unpack src
  (pyMod,_) <- parseModule src' fp    ? paErr §§ "Parse Python source"
  pyModTy   <- infer pyMod            ? tyErr §§ "Infer Python types"
  cfg       <- CFG.fromModule pyModTy ? pyErr §§ "Construct Python CFG"
  let dom    = domTree cfg
  prog      <- transpile dom          ? pyErr §§ "Transpile Python to Panini"
  prog2     <- inlineProgram prog              § "Inline bindings"
  module_   <- liftIO $ getModule fp
  return     $ (module_, prog2)
 where
  paErr = pyErr . Py.ParserError
  tyErr = pyErr . Py.TypeError
  pyErr = PythonFrontendError
