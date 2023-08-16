assert : { b:bool | b = true } -> unit

-- TODO: I'm actually not sure what this should infer, given the type signature
f : {s:string|false} -> unit
f = \s:string. assert false
