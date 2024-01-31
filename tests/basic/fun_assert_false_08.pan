assert : { b:𝔹 | b = true } → 𝟙

f10 : unit -> unit
f10 = \x:unit. assert false

f11 : bool -> unit
f11 = \x:bool. assert false

f12 : int -> unit
f12 = \x:int. assert false

f13 : string -> unit
f13 = \x:string. assert false
