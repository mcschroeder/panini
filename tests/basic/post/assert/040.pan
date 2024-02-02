assert : { b:𝔹 | b = true } → 𝟙

f40 : {x:bool|x=true} -> {v:unit|?}
f40 = \x:bool. assert x
