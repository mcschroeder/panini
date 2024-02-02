and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f50 : {x:bool|x=true} -> {y:bool|?} -> {z:bool|?}
f50 = \x:bool. \y:bool. and x y

f51 : {x:bool|x=false} -> {y:bool|?} -> {z:bool|?}
f51 = \x:bool. \y:bool. and x y

f52 : {x:bool|true} -> {y:bool|?} -> {z:bool|?}
f52 = \x:bool. \y:bool. and x y

f53 : {x:bool|false} -> {y:bool|?} -> {z:bool|?}
f53 = \x:bool. \y:bool. and x y
