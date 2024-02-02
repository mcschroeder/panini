not : (x:bool) -> {y:bool|y = true <=> x = false}
assert : { b:𝔹 | b = true } → 𝟙

f150 : {b:bool|b=false} -> unit
f150 = \b:bool. let c = not b in assert c

