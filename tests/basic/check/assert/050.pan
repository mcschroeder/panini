assert : { b:𝔹 | b = true } → 𝟙

f50 : unit -> unit
f50 = \x:unit. assert true

f51 : bool -> unit
f51 = \x:bool. assert true

f52 : int -> unit
f52 = \x:int. assert true

f53 : string -> unit
f53 = \x:string. assert true
