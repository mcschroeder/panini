not : (x:bool) -> {y:bool|y = true <=> x = false}
assert : { b:𝔹 | b = true } → 𝟙

f60 : {b:bool|?} -> unit
f60 = \b:bool. let c = not b in assert c

