assert : { b:𝔹 | b = true } → 𝟙

f60 : {x:bool|false} -> {v:unit|?}
f60 = \x:bool. assert x

