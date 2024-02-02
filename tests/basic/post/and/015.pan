and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f15 : {x:bool|x=true} -> {y:bool|false} -> {z:bool|?}
f15 = \x:bool. \y:bool. and x y
