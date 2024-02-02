assert : { b:𝔹 | b = true } → 𝟙

f40 : {x:unit|?} -> {v:unit|false}
f40 = \x:unit. assert false

f41 : {x:bool|?} -> {v:unit|false}
f41 = \x:bool. assert false

f42 : {x:int|?} -> {v:unit|false}
f42 = \x:int. assert false

f43 : {x:string|?} -> {v:unit|false}
f43 = \x:string. assert false

