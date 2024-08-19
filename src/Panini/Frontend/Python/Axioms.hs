{-# LANGUAGE QuasiQuotes #-}

module Panini.Frontend.Python.Axioms where

import Panini.Frontend.Python.Typing.PyType
import Prelude
import Panini.Syntax
import Panini.Syntax.QQ

type Axiom = (String, Type)

axiomForFunction :: String -> [PyType] -> PyType -> Maybe Axiom
axiomForFunction fun args ret = case (fun,args,ret) of
  -- built-in functions
  ("and"    , [Bool, Bool], Bool) -> Just ("and", [panType| (a:𝔹) → (b:𝔹) → {c:𝔹 | c = true ⟺ (a = true ∧ b = true)} |])
  ("assert" , [Bool]      , None) -> Just ("assert", [panType| { b:𝔹 | b = true } → 𝟙 |])
  ("len"    , _           , _   ) -> axiomForFunction "__len__" args ret
  ("not"    , [Bool, Bool], Bool) -> Just ("not", [panType| (a:𝔹) → (b:𝔹) → {c:𝔹 | c = true ⟺ (a = true ∨ b = true)} |])
  ("or"     , [Bool, Bool], Bool) -> Just ("or", [panType| (a:𝔹) → {b:𝔹 | b = ¬a} |])

  -- comparisong methods
  ("__lt__", [Int, Int], Bool) -> Just ("lt", [panType| (a:ℤ) → (b:ℤ) → {c:𝔹 | c = true ⟺ a < b} |])
  ("__le__", [Int, Int], Bool) -> Just ("le", [panType| (a:ℤ) → (b:ℤ) → {c:𝔹 | c = true ⟺ a ≤ b} |])
  ("__eq__", [Int, Int], Bool) -> Just ("eq", [panType| (a:ℤ) → (b:ℤ) → {c:𝔹 | c = true ⟺ a = b} |])
  ("__eq__", [Str, Str], Bool) -> Just ("match", [panType| (s:𝕊) → (t:𝕊) → {b:𝔹 | b = true ⟺ s = t} |])
  ("__gt__", [Int, Int], Bool) -> Just ("gt", [panType| (a:ℤ) → (b:ℤ) → {c:𝔹 | c = true ⟺ a > b} |])
  ("__ge__", [Int, Int], Bool) -> Just ("ge", [panType| (a:ℤ) → (b:ℤ) → {c:𝔹 | c = true ⟺ a ≥ b} |])

  -- container methods
  ("__getitem__", [Str, Int], Str) -> Just ("slice1", [panType| (s:𝕊) → {i:ℤ | i ≥ 0 ∧ i < |s|} → {t:𝕊 | t = s[i..i]} |])
  ("__len__"    , [Str]     , Int) -> Just ("length", [panType| (s:𝕊) → {n:ℤ | n = |s|} |])
  
  -- numeric methods
  ("__add__", [Int, Int], Int) -> Just ("add", [panType| (a:ℤ) → (b:ℤ) → {c:ℤ | c = a + b} |])
  ("__sub__", [Int, Int], Int) -> Just ("sub", [panType| (a:ℤ) → (b:ℤ) → {c:ℤ | c = a - b} |])
  
  _ -> Nothing
