assert : { b:𝔹 | b = true } → 𝟙
not : (x:bool) -> {y:bool|y = ~x}

f0 = let b = not false in assert b

f1 = let b = not true in assert b     -- expected error

f2 : unit
f2 = let b = not false in assert b

f3 : unit                              -- expected error
f3 = let b = not true in assert b

f4 : {_:unit|?}
f4 = let b = not false in assert b

f5 : {_:unit|?}
f5 = let b = not true in assert b

f6 = \b:bool. let c = not b in assert c    -- expected error

f7 : bool -> unit                          -- expected error
f7 = \b:bool. let c = not b in assert c

f8 : {b:bool|?} -> unit
f8 = \b:bool. let c = not b in assert c
