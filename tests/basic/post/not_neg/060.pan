not : (x:bool) -> {y:bool|y = ~x}
assert : { b:𝔹 | b = true } → 𝟙

f60 : {v:unit|?}
f60 = let b = not false in assert b


