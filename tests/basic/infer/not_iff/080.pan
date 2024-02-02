not : (x:bool) -> {y:bool|y = true <=> x = false}
assert : { b:𝔹 | b = true } → 𝟙

f80 : {v:unit|?}
f80 = let b = not true in assert b

