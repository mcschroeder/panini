and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f20 : {x:bool|x=true} -> {y:bool|?} -> {z:bool|z=true}
f20 = \x:bool. \y:bool. and x y

f21 : {x:bool|x=true} -> {y:bool|?} -> {z:bool|z=false}
f21 = \x:bool. \y:bool. and x y

f22 : {x:bool|x=false} -> {y:bool|?} -> {z:bool|z=true}
f22 = \x:bool. \y:bool. and x y

f23 : {x:bool|x=false} -> {y:bool|?} -> {z:bool|z=false}
f23 = \x:bool. \y:bool. and x y
