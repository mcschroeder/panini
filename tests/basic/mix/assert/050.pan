assert : { b:𝔹 | b = true } → 𝟙

f50 : {x:unit|?} -> {v:unit|?}
f50 = \x:unit. assert true

f51 : {x:bool|?} -> {v:unit|?}
f51 = \x:bool. assert true

f52 : {x:int|?} -> {v:unit|?}
f52 = \x:int. assert true

f53 : {x:string|?} -> {v:unit|?}
f53 = \x:string. assert true
