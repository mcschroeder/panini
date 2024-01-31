assert : { b:𝔹 | b = true } → 𝟙
not : (x:bool) -> {y:bool|y = ~x}

f6 = \b:bool. let c = not b in assert c

f8 : {b:bool|?} -> unit
f8 = \b:bool. let c = not b in assert c

f9 : {b:bool|b=false} -> unit
f9 = \b:bool. let c = not b in assert c
