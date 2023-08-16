assert : { b:bool | b = true } -> unit

f : {s:string|true} -> unit
f = \s:string. assert false
