assert : { b:𝔹 | b = true } → 𝟙

f90 : {x:unit|x≠unit} -> unit
f90 = \x:unit. assert false

f91 : {x:bool|x=true /\ x=false} -> unit
f91 = \x:bool. assert false

f92 : {x:int|x>=0 /\ x < 0} -> unit
f92 = \x:int. assert false

f93 : {x:string|x="a" /\ x="b"} -> unit
f93 = \x:string. assert false
