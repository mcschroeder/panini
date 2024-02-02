not : (x:bool) -> {y:bool|y = true <=> x = false}
assert : { b:𝔹 | b = true } → 𝟙

f140 : unit
f140 = let b = not true in assert b

