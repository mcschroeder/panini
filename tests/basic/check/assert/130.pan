assert : { b:𝔹 | b = true } → 𝟙

f130 : {x:bool|x=false} -> unit
f130 = \x:bool. assert false

f131 : {x:bool|x=true} -> unit
f131 = \x:bool. assert false

f132 : {x:int|x=1} -> unit
f132 = \x:int. assert false

f133 : {x:int|x < 1} -> unit
f133 = \x:int. assert false

f134 : {x:string|x="a"} -> unit
f134 = \x:string. assert false

f135 : {x:string|x=""} -> unit
f135 = \x:string. assert false
