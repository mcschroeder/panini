assert : { b:𝔹 | b = true } → 𝟙

f72 : {x:bool|?} -> {v:unit|?}
f72 = \x:bool. assert x
