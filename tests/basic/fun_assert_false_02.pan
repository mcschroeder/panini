assert : { b:𝔹 | b = true } → 𝟙

f20 : {x:unit|?} -> {v:unit|?}
f20 = \x:unit. assert false

f21 : {x:bool|?} -> {v:unit|?}
f21 = \x:bool. assert false

f22 : {x:int|?} -> {v:unit|?}
f22 = \x:int. assert false

f23 : {x:string|?} -> {v:unit|?}
f23 = \x:string. assert false
