and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f65 : {x:bool|?} -> {y:bool|false} -> {z:bool|?}
f65 = \x:bool. \y:bool. and x y
