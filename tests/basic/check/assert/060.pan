assert : { b:𝔹 | b = true } → 𝟙

f60 : unit -> unit
f60 = \x:unit. assert false

f61 : bool -> unit
f61 = \x:bool. assert false

f62 : int -> unit
f62 = \x:int. assert false

f63 : string -> unit
f63 = \x:string. assert false
