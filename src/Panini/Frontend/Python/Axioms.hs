{-# LANGUAGE QuasiQuotes #-}

module Panini.Frontend.Python.Axioms where

import Panini.Frontend.Python.Typing.PyType
import Prelude
import Panini.Syntax
import Panini.Syntax.QQ

{-
Note [Slicing via __getitem__]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In reality, the __getitem__ method never takes more than two arguments,
including self; here, we allow for additional arguments representing possible
slice indexes. In Python, slicing of sequences is implemented via opaque slice
objects: an expression like s[i:j] translates to __getitem__(s,slice(i,j)).
Unfortunately, we cannot distinguish between slice objects with different
attribute types or recognize missing/default attributes purely on the (Python)
type level. But we need to implement different slice semantics based on the
number of given parameters, e.g., s[i:] should slice s from i to len(s) and
s[::2] should select every other item of s. To allow this, we simply "collapse"
the slice objects into additional parameters of __getitem__, with missing slice
attributes represented using the None type.

Note [Slice bounds]
~~~~~~~~~~~~~~~~~~~
In Python, a slice s[i:j] is defined as the sequence of items with index k such
that i <= k < j; in other words, the upper bound j is /exclusive/. This differs
from the s[i..j] function of Panini's refinement logic, which has an /inclusive/
upper bound.
-}

type Axiom = (String, Type)

axiomForFunction :: String -> [PyType] -> PyType -> Maybe Axiom
axiomForFunction fun args ret = case (fun,args,ret) of
  -- built-in functions
  ("and"    , [Bool, Bool], Bool) -> Just ("and", [panType| (a:𝔹) → (b:𝔹) → {c:𝔹 | c = true ⟺ (a = true ∧ b = true)} |])
  ("assert" , [Bool]      , None) -> Just ("assert", [panType| { b:𝔹 | b = true } → 𝟙 |])
  ("len"    , _           , _   ) -> axiomForFunction "__len__" args ret
  ("not"    , [Bool], Bool) -> Just ("or", [panType| (a:𝔹) → {b:𝔹 | b = ¬a} |])
  ("or"     , [Bool, Bool], Bool) -> Just ("not", [panType| (a:𝔹) → (b:𝔹) → {c:𝔹 | c = true ⟺ (a = true ∨ b = true)} |])

  -- comparison methods
  ("__lt__", [Int, Int], Bool) -> Just ("lt", [panType| (a:ℤ) → (b:ℤ) → {c:𝔹 | c = true ⟺ a < b} |])
  ("__le__", [Int, Int], Bool) -> Just ("le", [panType| (a:ℤ) → (b:ℤ) → {c:𝔹 | c = true ⟺ a ≤ b} |])
  ("__eq__", [Int, Int], Bool) -> Just ("eq", [panType| (a:ℤ) → (b:ℤ) → {c:𝔹 | c = true ⟺ a = b} |])
  ("__eq__", [Str, Str], Bool) -> Just ("match", [panType| (s:𝕊) → (t:𝕊) → {b:𝔹 | b = true ⟺ s = t} |])
  ("__gt__", [Int, Int], Bool) -> Just ("gt", [panType| (a:ℤ) → (b:ℤ) → {c:𝔹 | c = true ⟺ a > b} |])
  ("__ge__", [Int, Int], Bool) -> Just ("ge", [panType| (a:ℤ) → (b:ℤ) → {c:𝔹 | c = true ⟺ a ≥ b} |])

  -- container methods; see Note [Slicing via __getitem__] and Note [Slice bounds]
  ("__getitem__", [Str, Int]          , Str) -> Just ("slice1", [panType| (s:𝕊) → {i:ℤ | i ≥ 0 ∧ i < |s|} → {t:𝕊 | t = s[i..i]} |])
  ("__getitem__", [Str, Int, Int]     , Str) -> Just ("slice", [panType| (s:𝕊) → {i:ℤ | i ≥ 0 ∧ i < |s|} → {j:ℤ | i ≤ j  ∧ j < |s|} → {t:𝕊 | t = s[i..j-1]} |])
  ("__getitem__", [Str, Int, None]    , Str) -> Just ("sliceFrom", [panType| (s:𝕊) → {i:ℤ | i ≥ 0 ∧ i < |s|} → {t:𝕊 | t = s[i..|s|-1]} |])
  ("__getitem__", [Str, None, Int]    , Str) -> Just ("sliceTo", [panType| (s:𝕊) → {j:ℤ | j ≥ 0 ∧ j < |s|} → {t:𝕊 | t = s[0..j-1]} |])
  ("__getitem__", [Str, None, None]   , Str) -> Just ("strId", [panType| (s:𝕊) → {t:𝕊 | t = s} |])
  ("__len__"    , [Str]               , Int) -> Just ("length", [panType| (s:𝕊) → {n:ℤ | n = |s|} |])  
  ("index"      , [Str, Str]          , Int) -> Just ("index", [panType| (s:𝕊) → (t:𝕊) → {k:ℤ | k = str_indexof(s,t,0)}|])
  ("index"      , [Str, Str, Int]     , Int) -> Just ("indexFrom", [panType| (s:𝕊) → (t:𝕊) → {i:ℤ | i ≥ 0 ∧ i < |s|} → {k:ℤ | k = str_indexof(s,t,i)} |])

  -- numeric methods
  ("__add__", [Int, Int], Int) -> Just ("add", [panType| (a:ℤ) → (b:ℤ) → {c:ℤ | c = a + b} |])
  ("__sub__", [Int, Int], Int) -> Just ("sub", [panType| (a:ℤ) → (b:ℤ) → {c:ℤ | c = a - b} |])
  
  _ -> Nothing
