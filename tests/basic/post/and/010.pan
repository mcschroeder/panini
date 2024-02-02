and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f10 : {x:bool|x=true} -> {y:bool|y=true} -> {z:bool|?}
f10 = \x:bool. \y:bool. and x y

f11 : {x:bool|x=true} -> {y:bool|y=false} -> {z:bool|?}
f11 = \x:bool. \y:bool. and x y

f12 : {x:bool|x=true} -> {y:bool|y=x} -> {z:bool|?}
f12 = \x:bool. \y:bool. and x y




f14 : {x:bool|x=true} -> {y:bool|true} -> {z:bool|?}
f14 = \x:bool. \y:bool. and x y
