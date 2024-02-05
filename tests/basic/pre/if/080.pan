assert : { b:𝔹 | b = true } → 𝟙

f80 : {x:bool|?} -> unit
f80 = \x:bool. if x then assert true else assert true

f81 : {x:bool|?} -> unit
f81 = \x:bool. if x then assert true else assert false

f82 : {x:bool|?} -> unit
f82 = \x:bool. if x then assert false else assert true

f83 : {x:bool|?} -> unit
f83 = \x:bool. if x then assert false else assert false
