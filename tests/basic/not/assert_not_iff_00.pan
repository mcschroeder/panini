assert : { b:𝔹 | b = true } → 𝟙
not : (x:bool) -> {y:bool|y = true <=> x = false}

f0 = let b = not false in assert b

f2 : unit
f2 = let b = not false in assert b

f4 : {v:unit|?}
f4 = let b = not false in assert b
