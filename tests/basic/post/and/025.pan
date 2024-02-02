and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f25 : {x:bool|x=false} -> {y:bool|false} -> {z:bool|?}
f25 = \x:bool. \y:bool. and x y
