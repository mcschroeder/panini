assert : { b:𝔹 | b = true } → 𝟙

f30 : {x:unit|x≠unit} -> unit
f30 = \x:unit. assert false

f31 : {x:bool|x=true /\ x=false} -> unit
f31 = \x:bool. assert false

f32 : {x:int|x>=0 /\ x < 0} -> unit
f32 = \x:int. assert false

f33 : {x:string|x="a" /\ x="b"} -> unit
f33 = \x:string. assert false
