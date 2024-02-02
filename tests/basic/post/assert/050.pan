assert : { b:𝔹 | b = true } → 𝟙

f50 : {x:bool|x=false} -> {v:unit|?}
f50 = \x:bool. assert x

