assert : { b:𝔹 | b = true } → 𝟙

f0 = assert true

f1 = assert false    -- expected error

f2 : unit
f2 = assert true

f3 : unit            -- expected error
f3 = assert false

f4 : {_:unit|?}
f4 = assert true

f5 : {_:unit|?}
f5 = assert false

f6 = \b:bool. assert b     -- expected error

f7 : bool -> unit          -- expected error
f7 = \b:bool. assert b

f8 : {b:bool|?} -> unit
f8 = \b:bool. assert b

f9 : {b:bool|?} -> unit
f9 = \b:bool. assert true

f10 : {b:bool|?} -> unit
f10 = \b:bool. assert false

f11 = let b = true in assert b

f12 = let b = false in assert b        -- expected error

f13 : unit
f13 = let b = true in assert b

f14 : unit                           -- expected error
f14 = let b = false in assert b

f15 : {_:unit|?}
f15 = let b = true in assert b

f16 : {_:unit|?}
f16 = let b = false in assert b

f17 = \s:string. assert false   -- expected error

f18 : {s:string|?} -> unit
f18 = \s:string. assert false

f19 : {s:string|false} -> unit
f19 = \s:string. assert false

f20 : {s:string|true} -> unit   -- expected error
f20 = \s:string. assert false

f21 : {s:string|s = "hello"} -> unit    -- expected error
f21 = \s:string. assert false

f22 = \s:string. assert true

f23 : {s:string|?} -> unit
f23 = \s:string. assert true

f24 : {s:string|true} -> unit
f24 = \s:string. assert true

f25 : {s:string|false} -> unit
f25 = \s:string. assert true

f26 : {s:string|s = "hello"} -> unit
f26 = \s:string. assert true
