assert : { b:𝔹 | b = true } → 𝟙

f20 : {x:bool|x=false} -> {v:unit|?}
f20 = \x:bool. assert x

f21 : {x:bool|x=false} -> {v:unit|true}
f21 = \x:bool. assert x

f22 : {x:bool|x=false} -> {v:unit|false}
f22 = \x:bool. assert x

f23 : {x:bool|x=true} -> {v:unit|false}
f23 = \x:bool. assert x

f24 : {x:bool|true} -> {v:unit|false}
f24 = \x:bool. assert x