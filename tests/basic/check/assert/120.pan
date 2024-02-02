assert : { b:𝔹 | b = true } → 𝟙

f120 : {x:unit|x=unit} -> unit
f120 = \x:unit. assert true

f121 : {x:bool|x=true} -> unit
f121 = \x:bool. assert true

f122 : {x:bool|x=false} -> unit
f122 = \x:bool. assert true

f123 : {x:int|x=1} -> unit
f123 = \x:int. assert true

f124 : {x:int|x<0} -> unit
f124 = \x:int. assert true

f125 : {x:string|x="a"} -> unit
f125 = \x:string. assert true

f126 : {x:string|x=""} -> unit
f126 = \x:string. assert true
