module Panini.Frontend.Python.Typing.TypeInfo where

import Data.Maybe
import Panini.Frontend.Python.AST
import Panini.Frontend.Python.Typing.PyType (PyType)
import Panini.Frontend.Python.Typing.PyType qualified as PyType
import Prelude

type TypeInfo = Maybe PyType

type Typed t a = t (TypeInfo, a)

typeInfoOf :: Annotated t => Typed t a -> TypeInfo
typeInfoOf = fst . annot

typeOf :: Annotated t => Typed t a -> PyType
typeOf = fromMaybe PyType.Any . typeInfoOf
