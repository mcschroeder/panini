assert : { b:𝔹 | b = true } → 𝟙

f10 : {x:unit|false} -> {v:unit|?}
f10 = \x:unit. assert true

f11 : {x:bool|false} -> {v:unit|?}
f11 = \x:bool. assert true

f12 : {x:int|false} -> {v:unit|?}
f12 = \x:int. assert true

f13 : {x:string|false} -> {v:unit|?}
f13 = \x:string. assert true