assert : { b:𝔹 | b = true } → 𝟙

f00 = assert

f10 = \x:bool. assert x

----------------------------------------
-- these should all fail, because the functions go wrong for some input values

f20 : bool -> unit
f20 = \x:bool. assert x

f30 : {x:bool|true} -> unit
f30 = \x:bool. assert x

f40 : {x:bool|true} -> {v:unit|true}
f40 = \x:bool. assert x

f50 : {x:bool|true} -> {v:unit|v=unit}
f50 = \x:bool. assert x

f60 : {x:bool|true} -> {v:unit|false}
f60 = \x:bool. assert x

f70 : {x:bool|true} -> {v:unit|?}
f70 = \x:bool. assert x

----------------------------------------
-- these should not fail, because a function that cannot be called cannot go wrong

f31 : {x:bool|false} -> unit
f31 = \x:bool. assert x

f41 : {x:bool|false} -> {v:unit|true}
f41 = \x:bool. assert x

f51 : {x:bool|false} -> {v:unit|v=unit}
f51 = \x:bool. assert x

f61 : {x:bool|false} -> {v:unit|false}
f61 = \x:bool. assert x

f71 : {x:bool|false} -> {v:unit|?}
f71 = \x:bool. assert x

----------------------------------------

f32 : {x:bool|x=true} -> unit
f32 = \x:bool. assert x

f42 : {x:bool|x=true} -> {v:unit|true}
f42 = \x:bool. assert x

f52 : {x:bool|x=true} -> {v:unit|v=unit}
f52 = \x:bool. assert x

-- this one should fail, because when x is true, the functions cannot go wrong
f62 : {x:bool|x=true} -> {v:unit|false}
f62 = \x:bool. assert x

f72 : {x:bool|x=true} -> {v:unit|?}
f72 = \x:bool. assert x

----------------------------------------
-- these should all fail, because if x is false the function always goes wrong

f33 : {x:bool|x=false} -> unit
f33 = \x:bool. assert x

f43 : {x:bool|x=false} -> {v:unit|true}
f43 = \x:bool. assert x

f53 : {x:bool|x=false} -> {v:unit|v=unit}
f53 = \x:bool. assert x

f63 : {x:bool|x=false} -> {v:unit|false}
f63 = \x:bool. assert x

f73 : {x:bool|x=false} -> {v:unit|?}
f73 = \x:bool. assert x

----------------------------------------

f34 : {x:bool|?} -> unit
f34 = \x:bool. assert x

f44 : {x:bool|?} -> {v:unit|true}
f44 = \x:bool. assert x

f54 : {x:bool|?} -> {v:unit|v=unit}
f54 = \x:bool. assert x

-- this should infer "false" for x (NOT "x = false") 
-- because no possible input can lead to an impossible output
f64 : {x:bool|?} -> {v:unit|false}
f64 = \x:bool. assert x

f74 : {x:bool|?} -> {v:unit|?}
f74 = \x:bool. assert x
