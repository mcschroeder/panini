assert : { b:𝔹 | b = true } → 𝟙

f30 : unit -> {v:unit|?}
f30 = \x:unit. assert false

f31 : bool -> {v:unit|?}
f31 = \x:bool. assert false

f32 : int -> {v:unit|?}
f32 = \x:int. assert false

f33 : string -> {v:unit|?}
f33 = \x:string. assert false

