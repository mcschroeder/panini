assert : { b:𝔹 | b = true } → 𝟙

f20 : {x:unit|false} -> {v:unit|?}
f20 = \x:unit. assert false

f21 : {x:bool|false} -> {v:unit|?}
f21 = \x:bool. assert false

f22 : {x:int|false} -> {v:unit|?}
f22 = \x:int. assert false

f23 : {x:string|false} -> {v:unit|?}
f23 = \x:string. assert false
