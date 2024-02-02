not : (x:bool) -> {y:bool|y = ~x}
assert : { b:𝔹 | b = true } → 𝟙

f70 = let b = not true in assert b


