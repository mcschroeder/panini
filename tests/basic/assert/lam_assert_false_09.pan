assert : { b:𝔹 | b = true } → 𝟙

f20 : unit -> {v:unit|false}
f20 = \x:unit. assert false

f21 : bool -> {v:unit|false}
f21 = \x:bool. assert false

f22 : int -> {v:unit|false}
f22 = \x:int. assert false

f23 : string -> {v:unit|false}
f23 = \x:string. assert false
