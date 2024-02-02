assert : { b:𝔹 | b = true } → 𝟙

f140 : {x:bool|x=true} -> unit
f140 = \x:bool. assert x
