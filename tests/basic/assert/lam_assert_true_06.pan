assert : { b:𝔹 | b = true } → 𝟙

f30 : {x:unit|x=unit} -> unit
f30 = \x:unit. assert true

f31a : {x:bool|x=true} -> unit
f31a = \x:bool. assert true

f31b : {x:bool|x=false} -> unit
f31b = \x:bool. assert true

f32a : {x:int|x=1} -> unit
f32a = \x:int. assert true

f32b : {x:int|x<0} -> unit
f32b = \x:int. assert true

f33a : {x:string|x="a"} -> unit
f33a = \x:string. assert true

f33b : {x:string|x="a"} -> unit
f33b = \x:string. assert true
