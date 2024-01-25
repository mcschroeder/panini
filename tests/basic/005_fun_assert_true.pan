assert : { b:𝔹 | b = true } → 𝟙

f00 = \x:int. assert true

----------------------------------------

f10 : int -> unit
f10 = \x:int. assert true

f12 : {x:int|?} -> unit
f12 = \x:int. assert true

f14 : {x:int|false} -> unit
f14 = \x:int. assert true

f16 : {x:int|x=1} -> unit
f16 = \x:int. assert true

----------------------------------------

f20 : int -> {v:unit|true}
f20 = \x:int. assert true

f22 : {x:int|?} -> {v:unit|true}
f22 = \x:int. assert true

f24 : {x:int|false} -> {v:unit|true}
f24 = \x:int. assert true

f26 : {x:int|x=1} -> {v:unit|true}
f26 = \x:int. assert true

----------------------------------------

f30 : int -> {v:unit|v=unit}
f30 = \x:int. assert true

f32 : {x:int|?} -> {v:unit|v=unit}
f32 = \x:int. assert true

f34 : {x:int|false} -> {v:unit|v=unit}
f34 = \x:int. assert true

f36 : {x:int|x=1} -> {v:unit|v=unit}
f36 = \x:int. assert true

----------------------------------------

f40 : int -> {v:unit|v≠unit}
f40 = \x:int. assert true

f42 : {x:int|?} -> {v:unit|v≠unit}
f42 = \x:int. assert true

f44 : {x:int|false} -> {v:unit|v≠unit}
f44 = \x:int. assert true

f46 : {x:int|x=1} -> {v:unit|v≠unit}
f46 = \x:int. assert true

----------------------------------------

f50 : int -> {v:unit|false}
f50 = \x:int. assert true

f52 : {x:int|?} -> {v:unit|false}
f52 = \x:int. assert true

f54 : {x:int|false} -> {v:unit|false}
f54 = \x:int. assert true

f56 : {x:int|x=1} -> {v:unit|false}
f56 = \x:int. assert true

----------------------------------------

f60 : int -> {v:unit|?}
f60 = \x:int. assert true

f62 : {x:int|?} -> {v:unit|?}
f62 = \x:int. assert true

f64 : {x:int|false} -> {v:unit|?}
f64 = \x:int. assert true

f66 : {x:int|x=1} -> {v:unit|?}
f66 = \x:int. assert true
