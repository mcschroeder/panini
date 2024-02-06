and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f50 : {x:bool|x=true} -> {y:bool|?} -> {z:bool|z=true}
f50 = \x:bool. \y:bool. and x y

f51 : {x:bool|x=false} -> {y:bool|?} -> {z:bool|z=true}
f51 = \x:bool. \y:bool. and x y

f52 : {x:bool|true} -> {y:bool|?} -> {z:bool|z=true}
f52 = \x:bool. \y:bool. and x y

f53 : {x:bool|false} -> {y:bool|?} -> {z:bool|z=true}
f53 = \x:bool. \y:bool. and x y
