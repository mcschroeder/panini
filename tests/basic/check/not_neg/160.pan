not : (x:bool) -> {y:bool|y = ~x}
assert : { b:𝔹 | b = true } → 𝟙

f160 : {b:bool|b=true} -> unit
f160 = \b:bool. let c = not b in assert c

f161 : {b:bool|true} -> unit
f161 = \b:bool. let c = not b in assert c
