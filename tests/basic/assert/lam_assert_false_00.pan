assert : { b:𝔹 | b = true } → 𝟙

f00 = \x:unit. assert false

f01 = \x:bool. assert false

f02 = \x:int. assert false

f03 = \x:string. assert false
