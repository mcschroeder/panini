not : (x:bool) -> {y:bool|y = true <=> x = false}
assert : { b:𝔹 | b = true } → 𝟙

f60 : {v:unit|?}
f60 = let b = not false in assert b


