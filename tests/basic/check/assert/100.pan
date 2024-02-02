assert : { b:𝔹 | b = true } → 𝟙

f100 : unit -> {v:unit|false}
f100 = \x:unit. assert true

f101 : bool -> {v:unit|false}
f101 = \x:bool. assert true

f102 : int -> {v:unit|false}
f102 = \x:int. assert true

f103 : string -> {v:unit|false}
f103 = \x:string. assert true
