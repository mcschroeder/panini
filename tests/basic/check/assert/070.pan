assert : { b:𝔹 | b = true } → 𝟙

f70 : {x:unit|false} -> unit
f70 = \x:unit. assert false

f71 : {x:bool|false} -> unit
f71 = \x:bool. assert false

f72 : {x:int|false} -> unit
f72 = \x:int. assert false

f73 : {x:string|false} -> unit
f73 = \x:string. assert false
