module Panini.Frontend.Python.Typing.Builtins
  ( builtinFunctions
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Panini.Frontend.Python.Typing.PyType
import Prelude

------------------------------------------------------------------------------

builtinFunctions :: Map String PyType
builtinFunctions = Map.fromListWith (\a b -> Union [a,b]) 
  $ globalFunctions
  <> concatMap constructors stubs
  <> concatMap functions stubs

------------------------------------------------------------------------------

-- TODO: without all of the generic definitions, e.g., for __getitem__, we might
-- be too eager in infering some (incorrect) concrete type!

-- TODO: generate these definitions automatically
-- TODO: support named parameters

-- based on https://github.com/python/typeshed/blob/main/stdlib/builtins.pyi

_T, _T_co, _T_contra, _KT, _VT, _SupportsNextT :: PyType
_T = TypeVar "_T"
_T_co = TypeVar "_T_co" -- TODO: covariant=true
_T_contra = TypeVar "_T_contra" -- TODO: contravariant=true
_KT = TypeVar "_KT"
_VT = TypeVar "_VT"
_SupportsNextT = TypeVar "_SupportsNextT" -- TODO: bound=SupportsNext[Any], covariant=True)

pattern ReadableBuffer :: PyType
pattern ReadableBuffer = Buffer

pattern ConvertibleToInt :: PyType
pattern ConvertibleToInt =
  Union [Str, ReadableBuffer, SupportsInt, SupportsIndex, SupportsTrunc]

pattern ConvertibleToFloat :: PyType
pattern ConvertibleToFloat = 
  Union [Str, ReadableBuffer, SupportsFloat, SupportsIndex]

data ClassStub = ClassStub
  { constructors :: [(String, PyType)]
  , attributes   :: [(String, PyType)]
  , functions    :: [(String, PyType)]
  }

stubs :: [ClassStub]
stubs = [objectStub,intStub,floatStub,complexStub,strStub,dictStub]

objectStub :: ClassStub
objectStub = ClassStub
  { constructors = []
  , attributes = []
  , functions =
    [ ("__setattr__", Callable [Object,Str,Any] None)
    , ("__delattr__", Callable [Object,Str] None)
    , ("__eq__", Callable [Object,Object] Bool)
    , ("__ne__", Callable [Object,Object] Bool)
    , ("__str__", Callable [Object] Str)
    , ("__repr__", Callable [Object] Str)
    , ("__hash__", Callable [Object] Int)
    , ("__format__", Callable [Object,Str] Str)
    , ("__getattribute__", Callable [Object,Str] Any)
    , ("__sizeof__", Callable [Object] Int)
    , ("__getstate__", Callable [Object] Object)  -- >= (3,11)
    , ("__dir__", Callable [Object] (Iterable Str))
    ]
  }

intStub :: ClassStub
intStub = ClassStub 
  { constructors =
    [ ("int", Callable [] Int)
    , ("int", Callable [ConvertibleToInt] Int)
    , ("int", Callable [Union [Str,Bytes,Bytearray], SupportsIndex] Int)
    ]
  , attributes =
    [ ("real", Int)
    , ("imag", Int)
    , ("numerator", Int)
    , ("denominator", Int)
    ]
  , functions =
    [ ("as_integer_ratio", Callable [Int] (Tuple [Int,Int]))
    , ("conjugate", Callable [Int] Int)
    , ("bit_length", Callable [Int] Int)
    , ("bit_count", Callable [Int] Int)  -- >=(3,10)
    , ("is_integer", Callable [Int] Bool) -- >=(3,12)
    , ("__add__", Callable [Int,Int] Int)
    , ("__sub__", Callable [Int,Int] Int)
    , ("__mul__", Callable [Int,Int] Int)
    , ("__floordiv__", Callable [Int,Int] Int)
    , ("__truediv__", Callable [Int,Int] Float)
    , ("__mod__", Callable [Int,Int] Int)
    , ("__divmod__", Callable [Int,Int] (Tuple [Int,Int]))
    , ("__radd__", Callable [Int,Int] Int)
    , ("__rsub__", Callable [Int,Int] Int)
    , ("__rmul__", Callable [Int,Int] Int)
    , ("__rfloordiv__", Callable [Int,Int] Int)
    , ("__rtruediv__", Callable [Int,Int] Float)
    , ("__rmod__", Callable [Int,Int] Int)
    , ("__rdivmod__", Callable [Int,Int] (Tuple[Int,Int]))
    , ("__pow__", Callable [Int,Int] (Union [Int,Float]))
    , ("__pow__", Callable [Int,Int,Int] Int)
    , ("__rpow__", Callable [Int,Int] (Union [Int,Float]))
    , ("__rpow__", Callable [Int,Int,Int] Int)
    , ("__and__", Callable [Int,Int] Int)
    , ("__or__", Callable [Int,Int] Int)
    , ("__xor__", Callable [Int,Int] Int)
    , ("__lshift__", Callable [Int,Int] Int)
    , ("__rshift__", Callable [Int,Int] Int)
    , ("__rand__", Callable [Int,Int] Int)
    , ("__ror__", Callable [Int,Int] Int)
    , ("__rxor__", Callable [Int,Int] Int)
    , ("__rlshift__", Callable [Int,Int] Int)
    , ("__rrshift__", Callable [Int,Int] Int)
    , ("__neg__", Callable [Int] Int)
    , ("__pos__", Callable [Int] Int)
    , ("__invert__", Callable [Int] Int)
    , ("__trunc__", Callable [Int] Int)
    , ("__ceil__", Callable [Int] Int)
    , ("__floor__", Callable [Int] Int)
    , ("__round__", Callable [Int] Int)
    , ("__round__", Callable [Int,SupportsIndex] Int)
    , ("__getnewargs__", Callable [Int] (Tuple [Int]))
    , ("__eq__", Callable [Int,Object] Bool)
    , ("__ne__", Callable [Int,Object] Bool)
    , ("__lt__", Callable [Int,Int] Bool)
    , ("__le__", Callable [Int,Int] Bool)
    , ("__gt__", Callable [Int,Int] Bool)
    , ("__ge__", Callable [Int,Int] Bool)
    , ("__float__", Callable [Int] Float)
    , ("__int__", Callable [Int] Int)
    , ("__abs__", Callable [Int] Int)
    , ("__hash__", Callable [Int] Int)
    , ("__bool__", Callable [Int] Bool)
    , ("__index__", Callable [Int] Int)
    ]
  }

floatStub :: ClassStub
floatStub = ClassStub
  { constructors = 
    [ ("float", Callable [] Float)
    , ("float", Callable [ConvertibleToFloat] Float)
    ]
  , attributes =
    [ ("real", Float)
    , ("imag", Float)
    ]
  , functions =
    [ ("as_integer_ratio", Callable [Float] (Tuple [Int,Int]))
    , ("hex", Callable [Float] Str)
    , ("is_integer", Callable [Float] Bool)
    , ("conjugate", Callable [Float] Float)
    , ("__add__", Callable [Float,Float] Float)
    , ("__sub__", Callable [Float,Float] Float)
    , ("__mul__", Callable [Float,Float] Float)
    , ("__floordiv__", Callable [Float,Float] Float)
    , ("__truediv__", Callable [Float,Float] Float)
    , ("__mod__", Callable [Float,Float] Float)
    , ("__divmod__", Callable [Float,Float] (Tuple [Float,Float]))
    , ("__pow__", Callable [Float,Int] Float)
    , ("__pow__", Callable [Float,Float] (Union [Float,Complex]))
    , ("__radd__", Callable [Float,Float] Float)
    , ("__rsub__", Callable [Float,Float] Float)
    , ("__rmul__", Callable [Float,Float] Float)
    , ("__rfloordiv__", Callable [Float,Float] Float)
    , ("__rtruediv__", Callable [Float,Float] Float)
    , ("__rmod__", Callable [Float,Float] Float)
    , ("__rdivmod__", Callable [Float,Float] (Tuple [Float,Float]))
    , ("__rpow__", Callable [Float,Int] (Union [Float,Complex]))
    , ("__rpow__", Callable [Float,Float] (Union [Float,Complex]))
    , ("__getnewargs__", Callable [Float] (Tuple [Float]))
    , ("__trunc__", Callable [Float] Int)
    , ("__ceil__", Callable [Float] Int) -- >=(3,9)
    , ("__floor__", Callable [Float] Int) -- >=(3,9)
    , ("__round__", Callable [Float] Int)
    , ("__round__", Callable [Float,SupportsIndex] Float)
    , ("__eq__", Callable [Float,Object] Bool)
    , ("__ne__", Callable [Float,Object] Bool)
    , ("__lt__", Callable [Float,Float] Bool)
    , ("__le__", Callable [Float,Float] Bool)
    , ("__gt__", Callable [Float,Float] Bool)
    , ("__ge__", Callable [Float,Float] Bool)
    , ("__neg__", Callable [Float] Float)
    , ("__pos__", Callable [Float] Float)
    , ("__int__", Callable [Float] Int)
    , ("__float__", Callable [Float] Float)
    , ("__abs__", Callable [Float] Float)
    , ("__hash__", Callable [Float] Int)
    , ("__bool__", Callable [Float] Bool)
    ]
  }

complexStub :: ClassStub
complexStub = ClassStub
  { constructors =
    [ ("complex", Callable [] Complex)
    , ("complex", Callable [Union [Str,Complex,SupportsComplex,SupportsFloat,SupportsIndex]] Complex)
    , ("complex", Callable [Union [Complex,SupportsComplex,SupportsFloat,SupportsIndex], Union [Complex,SupportsFloat,SupportsIndex]] Complex)
    ]
  , attributes =
    [ ("real", Float)
    , ("imag", Float)
    ]
  , functions =
    [ ("conjugate", Callable [Complex] Complex)
    , ("__add__", Callable [Complex,Complex] Complex)
    , ("__sub__", Callable [Complex,Complex] Complex)
    , ("__mul__", Callable [Complex,Complex] Complex)
    , ("__pow__", Callable [Complex,Complex] Complex)
    , ("__truediv__", Callable [Complex,Complex] Complex)
    , ("__radd__", Callable [Complex,Complex] Complex)
    , ("__rsub__", Callable [Complex,Complex] Complex)
    , ("__rmul__", Callable [Complex,Complex] Complex)
    , ("__rpow__", Callable [Complex,Complex] Complex)
    , ("__rtruediv__", Callable [Complex,Complex] Complex)
    , ("__eq__", Callable [Complex,Object] Bool)
    , ("__ne__", Callable [Complex,Object] Bool)
    , ("__neg__", Callable [Complex] Complex)
    , ("__pos__", Callable [Complex] Complex)
    , ("__abs__", Callable [Complex] Float)
    , ("__hash__", Callable [Complex] Int)
    , ("__bool__", Callable [Complex] Bool)
    , ("__complex__", Callable [Complex] Complex) -- >=(3,11)
    ]
  }

strStub :: ClassStub
strStub = ClassStub
  { constructors =
    [ ("str", Callable [] Str)
    , ("str", Callable [Object] Str)
    , ("str", Callable [ReadableBuffer] Str)
    , ("str", Callable [ReadableBuffer,Str] Str)
    , ("str", Callable [ReadableBuffer,Str,Str] Str)
    ]
  , attributes = []
  , functions =
    [ ("capitalize", Callable [Str] Str)
    , ("casefold", Callable [Str] Str)
    , ("center", Callable [Str,SupportsIndex] Str)
    , ("center", Callable [Str,SupportsIndex,Str] Str)
    , ("count", Callable [Str,Str] Int)
    , ("count", Callable [Str,Str,Optional SupportsIndex] Int)
    , ("count", Callable [Str,Str,Optional SupportsIndex,Optional SupportsIndex] Int)
    , ("encode", Callable [Str] Bytes)
    , ("encode", Callable [Str,Str] Bytes)
    , ("encode", Callable [Str,Str,Str] Bytes)
    , ("endswith", Callable [Str,Str] Bool)
    , ("endswith", Callable [Str,Str,Optional SupportsIndex] Bool)
    , ("endswith", Callable [Str,Str,Optional SupportsIndex,Optional SupportsIndex] Bool)    
    , ("expandtabs", Callable [Str] Str)
    , ("expandtabs", Callable [Str,SupportsIndex] Str)
    , ("find", Callable [Str,Str] Int)
    , ("find", Callable [Str,Str,Optional SupportsIndex] Int)
    , ("find", Callable [Str,Str,Optional SupportsIndex,Optional SupportsIndex] Int)
    , ("index", Callable [Str,Str] Int)
    , ("index", Callable [Str,Str,Optional SupportsIndex] Int)
    , ("index", Callable [Str,Str,Optional SupportsIndex,Optional SupportsIndex] Int)    
    , ("isalnum", Callable [Str] Bool)
    , ("isalpha", Callable [Str] Bool)
    , ("isascii", Callable [Str] Bool)
    , ("isdecimal", Callable [Str] Bool)
    , ("isdigit", Callable [Str] Bool)
    , ("isidentifier", Callable [Str] Bool)
    , ("islower", Callable [Str] Bool)
    , ("isnumeric", Callable [Str] Bool)
    , ("isprintable", Callable [Str] Bool)
    , ("isspace", Callable [Str] Bool)
    , ("istitle", Callable [Str] Bool)
    , ("isupper", Callable [Str] Bool)
    , ("join", Callable [Str,Iterable Str] Str)
    , ("ljust", Callable [Str,SupportsIndex] Str)
    , ("ljust", Callable [Str,SupportsIndex,Str] Str)
    , ("lower", Callable [Str] Str)
    , ("lstrip", Callable [Str] Str)
    , ("lstrip", Callable [Str, Optional Str] Str)
    , ("partition", Callable [Str,Str] (Tuple [Str,Str,Str]))
    , ("replace", Callable [Str,Str,Str] Str)
    , ("replace", Callable [Str,Str,Str,SupportsIndex] Str)
    , ("removeprefix", Callable [Str,Str] Str) -- >=(3,9)
    , ("removesuffix", Callable [Str,Str] Str) -- >=(3,9)
    , ("rfind", Callable [Str,Str] Int)
    , ("rfind", Callable [Str,Str,Optional SupportsIndex] Int)
    , ("rfind", Callable [Str,Str,Optional SupportsIndex,Optional SupportsIndex] Int)
    , ("rindex", Callable [Str,Str] Int)
    , ("rindex", Callable [Str,Str,Optional SupportsIndex] Int)
    , ("rindex", Callable [Str,Str,Optional SupportsIndex,Optional SupportsIndex] Int)
    , ("rjust", Callable [Str,SupportsIndex] Str)
    , ("rjust", Callable [Str,SupportsIndex,Str] Str)
    , ("rpartition", Callable [Str,Str] (Tuple [Str,Str,Str]))
    , ("rsplit", Callable [Str] (List Str))
    , ("rsplit", Callable [Str,Optional Str] (List Str))
    , ("rsplit", Callable [Str,Optional Str,SupportsIndex] (List Str))
    , ("rstrip", Callable [Str] Str)
    , ("rstrip", Callable [Str,Optional Str] Str)
    , ("split", Callable [Str] (List Str))
    , ("split", Callable [Str,Optional Str] (List Str))
    , ("split", Callable [Str,Optional Str,SupportsIndex] (List Str))
    , ("splitlines", Callable [Str] (List Str))
    , ("splitlines", Callable [Str,Bool] (List Str))
    , ("startswith", Callable [Str,Str] Bool)
    , ("startswith", Callable [Str,Str,Optional SupportsIndex] Bool)
    , ("startswith", Callable [Str,Str,Optional SupportsIndex,Optional SupportsIndex] Bool)
    , ("strip", Callable [Str] Str)
    , ("strip", Callable [Str,Optional Str] Str)
    , ("swapcase", Callable [Str] Str)
    , ("title", Callable [Str] Str)
    , ("upper", Callable [Str] Str)
    , ("zfill", Callable [Str,SupportsIndex] Str)
    , ("__add__", Callable [Str,Str] Str)
    , ("__contains__", Callable [Str,Str] Bool)
    , ("__eq__", Callable [Str,Object] Bool)
    , ("__ge__", Callable [Str,Str] Bool)
    , ("__getitem__", Callable [Str, Union [SupportsIndex,Slice]] Str)
    , ("__gt__", Callable [Str,Str] Bool)
    , ("__hash__", Callable [Str] Int)
    , ("__iter__", Callable [Str] (Iterator Str))
    , ("__le__", Callable [Str,Str] Bool)
    , ("__len__", Callable [Str] Int)
    , ("__lt__", Callable [Str,Str] Bool)
    , ("__mod__", Callable [Str,Any] Str)
    , ("__mul__", Callable [Str,SupportsIndex] Str)
    , ("__ne__", Callable [Str,Object] Bool)
    , ("__rmul__", Callable [Str,SupportsIndex] Str)
    , ("__getnewargs__", Callable [Str] (Tuple [Str]))
    ]
  }

dictStub :: ClassStub
dictStub = ClassStub
  { constructors = []  -- TODO
  , attributes = []
  , functions =
    [ ("__getitem__", Callable [Dict _KT _VT, _KT] _VT)
    , ("__contains__", Callable [Dict _KT _VT, Any] Bool)
    -- TODO
    ] 
  }

globalFunctions :: [(String,PyType)]
globalFunctions =
  [ ("abs", Callable [SupportsAbs _T] _T)
  , ("all", Callable [Iterable Object] Bool)
  , ("any", Callable [Iterable Object] Bool)
  , ("ascii", Callable [Object] Str)
  , ("bin", Callable [Union [Int,SupportsIndex]] Str)
  , ("callable", Callable [Object] Bool)
  , ("chr", Callable [Int] Str)
  , ("delattr", Callable [Object,Str] None)
  , ("dir", Callable [] (List Str))
  , ("dir", Callable [Object] (List Str))
  , ("divmod", Callable [SupportsDivMod _T_contra _T_co, _T_contra] _T_co)
  , ("divmod", Callable [_T_contra, SupportsRDivMod _T_contra _T_co] _T_co)
  , ("exit", Callable [] NoReturn)
  , ("exit", Callable [Any] NoReturn)
  , ("getattr", Callable [Object,Str] Any)
  , ("globals", Callable [] (Dict Str Any))
  , ("hasattr", Callable [Object,Str] Bool)
  , ("hash", Callable [Object] Int)
  , ("hex", Callable [Union [Int,SupportsIndex]] Str)
  , ("id", Callable [Object] Int)
  , ("input", Callable [] Str)
  , ("input", Callable [Object] Str)
  , ("iter", Callable [SupportsIter _SupportsNextT] _SupportsNextT)
  , ("iter", Callable [Callable [] (Optional _T), None] (Iterator _T))
  , ("iter", Callable [Callable [] _T, Object] (Iterator _T))
  , ("len", Callable [Sized] Int)
  , ("locals", Callable [] (Dict Str Any))
  , ("max", Callable [_T,_T] _T)
  , ("max", Callable [Iterable _T] _T)
  , ("min", Callable [_T,_T] _T)
  , ("min", Callable [Iterable _T] _T)
  , ("next", Callable [SupportsNext _T] _T)
  , ("next", Callable [SupportsNext _T, _VT] (Union [_T,_VT]))
  , ("oct", Callable [Union [Int,SupportsIndex]] Str)
  , ("pow", Callable [Int,Int,Int] Int)
  , ("pow", Callable [Int,Int] (Union [Int,Float]))
  , ("pow", Callable [Float,Int] Float)
  , ("pow", Callable [Int,Float] (Union [Float,Complex]))
  , ("pow", Callable [Float,Float] (Union [Float,Complex]))
  , ("pow", Callable [Float,Complex] (Union [Float,Complex]))
  , ("pow", Callable [Complex,Complex] Complex)
  , ("quit", Callable [] NoReturn)
  , ("quit", Callable [Any] NoReturn)
  , ("repr", Callable [Object] Str)
  , ("round", Callable [SupportsRound _T] _T)
  , ("round", Callable [SupportsRound _T, SupportsIndex] _T)
  , ("sorted", Callable [Iterable _T] (List _T))
  , ("sorted", Callable [Iterable _T,Bool] (List _T))
  , ("sorted", Callable [Iterable _T,None,Bool] (List _T))
  , ("sorted", Callable [Iterable _T,Callable [_T] Object,Bool] (List _T))
  , ("sum", Callable [Iterable (Union [Bool,Int])] Int)
  , ("sum", Callable [Iterable (Union [Bool,Int]),Int] Int)
  , ("sum", Callable [Iterable Any] Any)
  , ("vars", Callable [] (Dict Str Any))
  , ("vars", Callable [Any] (Dict Str Any))
  ]