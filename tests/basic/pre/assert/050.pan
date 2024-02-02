assert : { b:𝔹 | b = true } → 𝟙

f50 : {x:bool|?} -> unit
f50 = \x:bool. assert x
