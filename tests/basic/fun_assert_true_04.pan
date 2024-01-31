assert : { b:𝔹 | b = true } → 𝟙

f40 : {x:unit|false} -> {v:unit|?}
f40 = \x:unit. assert true

f41 : {x:bool|false} -> {v:unit|?}
f41 = \x:bool. assert true

f42 : {x:int|false} -> {v:unit|?}
f42 = \x:int. assert true

f43 : {x:string|false} -> {v:unit|?}
f43 = \x:string. assert true
