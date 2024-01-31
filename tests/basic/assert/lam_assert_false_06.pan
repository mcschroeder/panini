assert : { b:𝔹 | b = true } → 𝟙

f20 : {x:unit|false} -> {v:unit|false}
f20 = \x:unit. assert false

f21 : {x:bool|false} -> {v:unit|false}
f21 = \x:bool. assert false

f22 : {x:int|false} -> {v:unit|false}
f22 = \x:int. assert false

f23 : {x:string|false} -> {v:unit|false}
f23 = \x:string. assert false
