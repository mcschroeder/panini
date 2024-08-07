module Panini.Frontend.Python.Axioms where

import Panini.Frontend.Python.AST (Op(..))
import Panini.Frontend.Python.Typing.PyType
import Panini.Frontend.Python.Typing.TypeInfo
import Prelude

axiomForFunction :: String -> [PyType] -> PyType -> Maybe String
axiomForFunction fun args ret = case (fun,args,ret) of
  ("len", [Str], Int) -> Just "length"
  _ -> Nothing

axiomForOperator :: Typed Op a -> Maybe String
axiomForOperator op = case (op, typeOf op) of
  (LessThan{} , Callable [Int , Int] Bool ) -> Just "lt"
  (Equality{} , Callable [Int , Int] Bool ) -> Just "eq"
  (Equality{} , Callable [Str , Str] Bool ) -> Just "match"
  (Plus{}     , Callable [Int , Int] Int  ) -> Just "add"
  (Minus{}    , Callable [Int , Int] Int  ) -> Just "sub"  
  _                                         -> Nothing

axiomForSubscript :: PyType -> PyType -> PyType -> Maybe String
axiomForSubscript s i r = case (s,i,r) of
  (Str, Int, Str) -> Just "charAt" -- TODO
  _               -> Nothing
