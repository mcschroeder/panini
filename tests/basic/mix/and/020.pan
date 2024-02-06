and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f20 : {x:bool|x=true} -> {y:bool|?} -> {z:bool|?}
f20 = \x:bool. \y:bool. and x y

f21 : {x:bool|x=false} -> {y:bool|?} -> {z:bool|?}
f21 = \x:bool. \y:bool. and x y

f22 : {x:bool|true} -> {y:bool|?} -> {z:bool|?}
f22 = \x:bool. \y:bool. and x y

f23 : {x:bool|false} -> {y:bool|?} -> {z:bool|?}
f23 = \x:bool. \y:bool. and x y
