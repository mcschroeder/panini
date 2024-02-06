and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f33 : {x:bool|?} -> {y:bool|y/=x} -> {z:bool|?}
f33 = \x:bool. \y:bool. and x y