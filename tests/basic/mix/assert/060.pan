assert : { b:𝔹 | b = true } → 𝟙

f60 : {x:unit|?} -> {v:unit|?}
f60 = \x:unit. assert false

f61 : {x:bool|?} -> {v:unit|?}
f61 = \x:bool. assert false

f62 : {x:int|?} -> {v:unit|?}
f62 = \x:int. assert false

f63 : {x:string|?} -> {v:unit|?}
f63 = \x:string. assert false
