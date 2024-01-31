assert : { b:𝔹 | b = true } → 𝟙

f30 : {x:unit|?} -> {v:unit|false}
f30 = \x:unit. assert true

f31 : {x:bool|?} -> {v:unit|false}
f31 = \x:bool. assert true

f32 : {x:int|?} -> {v:unit|false}
f32 = \x:int. assert true

f33 : {x:string|?} -> {v:unit|false}
f33 = \x:string. assert true
