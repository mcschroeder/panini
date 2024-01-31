assert : { b:𝔹 | b = true } → 𝟙

f00 = assert

f10 = \x:bool. assert x

f20 : {x:bool|?} -> {v:unit|?}
f20 = \x:bool. assert x

f21 : {x:bool|x=true} -> {v:unit|?}
f21 = \x:bool. assert x

f31 : {x:bool|?} -> unit
f31 = \x:bool. assert x

f32 : {x:bool|x=true} -> unit
f32 = \x:bool. assert x
