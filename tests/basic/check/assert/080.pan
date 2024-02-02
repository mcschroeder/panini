assert : { b:𝔹 | b = true } → 𝟙

f80 : {x:unit|false} -> {v:unit|false}
f80 = \x:unit. assert false

f81 : {x:bool|false} -> {v:unit|false}
f81 = \x:bool. assert false

f82 : {x:int|false} -> {v:unit|false}
f82 = \x:int. assert false

f83 : {x:string|false} -> {v:unit|false}
f83 = \x:string. assert false
