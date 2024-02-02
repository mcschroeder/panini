assert : { b:𝔹 | b = true } → 𝟙

f150 : {x:bool|x=false} -> {v:unit|true}
f150 = \x:bool. assert x

f151 : {x:bool|x=false} -> {v:unit|false}
f151 = \x:bool. assert x

f152 : {x:bool|x=true} -> {v:unit|false}
f152 = \x:bool. assert x

f153 : {x:bool|true} -> {v:unit|false}
f153 = \x:bool. assert x
