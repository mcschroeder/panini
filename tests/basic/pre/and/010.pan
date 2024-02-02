and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f10 : {x:bool|?} -> {y:bool|?} -> {z:bool|z=true}
f10 = \x:bool. \y:bool. and x y
