module Panini.Frontend.Python.Axioms where

import Panini.Frontend.Python.Typing.PyType
import Prelude

axiomForFunction :: String -> [PyType] -> PyType -> Maybe String
axiomForFunction fun args ret = case (fun,args,ret) of
  -- built-in functions
  ("and"  , [Bool, Bool], Bool) -> Just "and"  
  ("len"  , _           , _   ) -> axiomForFunction "__len__" args ret
  ("not"  , [Bool, Bool], Bool) -> Just "not"
  ("or"   , [Bool, Bool], Bool) -> Just "or"

  -- comparisong methods
  ("__lt__", [Int, Int], Bool) -> Just "lt"
  ("__le__", [Int, Int], Bool) -> Just "le"
  ("__eq__", [Int, Int], Bool) -> Just "eq"
  ("__eq__", [Str, Str], Bool) -> Just "match"
  ("__gt__", [Int, Int], Bool) -> Just "gt"
  ("__ge__", [Int, Int], Bool) -> Just "ge"

  -- container methods
  ("__getitem__", [Str, Int], Str) -> Just "slice1"
  ("__len__"    , [Str]     , Int) -> Just "length"
  
  -- numeric methods
  ("__add__", [Int, Int], Int) -> Just "add"
  ("__sub__", [Int, Int], Int) -> Just "sub"
  
  _ -> Nothing
