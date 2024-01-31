assert : { b:𝔹 | b = true } → 𝟙

f10 : unit -> {v:unit|false}
f10 = \x:unit. assert true

f11 : bool -> {v:unit|false}
f11 = \x:bool. assert true

f12 : int -> {v:unit|false}
f12 = \x:int. assert true

f13 : string -> {v:unit|false}
f13 = \x:string. assert true
