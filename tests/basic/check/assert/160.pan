assert : { b:𝔹 | b = true } → 𝟙

f160 : {x:bool|false} -> {v:unit|true}
f160 = \x:bool. assert x

f161 : {x:bool|false} -> {v:unit|false}
f161 = \x:bool. assert x
