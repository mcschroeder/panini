assert : { b:𝔹 | b = true } → 𝟙

f110 : unit -> {v:unit|false}
f110 = \x:unit. assert false

f111 : bool -> {v:unit|false}
f111 = \x:bool. assert false

f112 : int -> {v:unit|false}
f112 = \x:int. assert false

f113 : string -> {v:unit|false}
f113 = \x:string. assert false

-- NOTE
-- these should not type check, because "assert false" does not type check
