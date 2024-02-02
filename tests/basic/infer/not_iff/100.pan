not : (x:bool) -> {y:bool|y = true <=> x = false}
assert : { b:𝔹 | b = true } → 𝟙

f100 : {b:bool|?} -> {v:unit|?}
f100 = \b:bool. let c = not b in assert c

