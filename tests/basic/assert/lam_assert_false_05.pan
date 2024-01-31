assert : { b:𝔹 | b = true } → 𝟙

f10 : {x:unit|false} -> unit
f10 = \x:unit. assert false

f11 : {x:bool|false} -> unit
f11 = \x:bool. assert false

f12 : {x:int|false} -> unit
f12 = \x:int. assert false

f13 : {x:string|false} -> unit
f13 = \x:string. assert false
