assert : { b:𝔹 | b = true } → 𝟙

f41a : {x:bool|x=false} -> unit
f41a = \x:bool. assert false

f41b : {x:bool|x=true} -> unit
f41b = \x:bool. assert false

f42a : {x:int|x=1} -> unit
f42a = \x:int. assert false

f42b : {x:int|x < 1} -> unit
f42b = \x:int. assert false

f43 : {x:string|x=""} -> unit
f43 = \x:string. assert false

f43b : {x:string|x="a"} -> unit
f43b = \x:string. assert false
