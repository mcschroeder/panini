assert : { b:𝔹 | b = true } → 𝟙

f70 = assert

f71 = \x:bool. assert x

f72 : {x:bool|?} -> {v:unit|?}
f72 = \x:bool. assert x

