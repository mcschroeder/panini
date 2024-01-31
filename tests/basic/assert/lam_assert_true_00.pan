assert : { b:𝔹 | b = true } → 𝟙

f00 = \x:unit. assert true

f01 = \x:bool. assert true

f02 = \x:int. assert true

f03 = \x:string. assert true
