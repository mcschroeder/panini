not : (x:bool) -> {y:bool|y = ~x}
assert : { b:𝔹 | b = true } → 𝟙

f140 : unit
f140 = let b = not true in assert b

