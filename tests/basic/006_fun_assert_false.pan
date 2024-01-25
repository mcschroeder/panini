assert : { b:𝔹 | b = true } → 𝟙

f01 = \x:int. assert false

----------------------------------------

f11 : int -> unit
f11 = \x:int. assert false

f13 : {x:int|?} -> unit
f13 = \x:int. assert false

f15 : {x:int|false} -> unit
f15 = \x:int. assert false

f17 : {x:int|x=1} -> unit
f17 = \x:int. assert false

----------------------------------------

f21 : int -> {v:unit|true}
f21 = \x:int. assert false

f23 : {x:int|?} -> {v:unit|true}
f23 = \x:int. assert false

f25 : {x:int|false} -> {v:unit|true}
f25 = \x:int. assert false

f27 : {x:int|x=1} -> {v:unit|true}
f27 = \x:int. assert false

----------------------------------------

f31 : int -> {v:unit|v=unit}
f31 = \x:int. assert false

f33 : {x:int|?} -> {v:unit|v=unit}
f33 = \x:int. assert false

f35 : {x:int|false} -> {v:unit|v=unit}
f35 = \x:int. assert false

f37 : {x:int|x=1} -> {v:unit|v=unit}
f37 = \x:int. assert false

----------------------------------------

f41 : int -> {v:unit|v≠unit}
f41 = \x:int. assert false

f43 : {x:int|?} -> {v:unit|v≠unit}
f43 = \x:int. assert false

f45 : {x:int|false} -> {v:unit|v≠unit}
f45 = \x:int. assert false

f47 : {x:int|x=1} -> {v:unit|v≠unit}
f47 = \x:int. assert false

----------------------------------------

f51 : int -> {v:unit|false}
f51 = \x:int. assert false

f53 : {x:int|?} -> {v:unit|false}
f53 = \x:int. assert false

f55 : {x:int|false} -> {v:unit|false}
f55 = \x:int. assert false

f57 : {x:int|x=1} -> {v:unit|false}
f57 = \x:int. assert false

----------------------------------------

f61 : int -> {v:unit|?}
f61 = \x:int. assert false

f63 : {x:int|?} -> {v:unit|?}
f63 = \x:int. assert false

f65 : {x:int|false} -> {v:unit|?}
f65 = \x:int. assert false

f67 : {x:int|x=1} -> {v:unit|?}
f67 = \x:int. assert false
