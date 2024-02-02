assert : { b:𝔹 | b = true } → 𝟙

f20 : {x:unit|?} -> unit
f20 = \x:unit. assert false

f21 : {x:bool|?} -> unit
f21 = \x:bool. assert false

f22 : {x:int|?} -> unit
f22 = \x:int. assert false

f23 : {x:string|?} -> unit
f23 = \x:string. assert false
