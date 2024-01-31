assert : { b:𝔹 | b = true } → 𝟙

f10 : {x:unit|?} -> unit
f10 = \x:unit. assert true

f11 : {x:bool|?} -> unit
f11 = \x:bool. assert true

f12 : {x:int|?} -> unit
f12 = \x:int. assert true

f13 : {x:string|?} -> unit
f13 = \x:string. assert true
