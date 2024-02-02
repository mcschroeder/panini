and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f13 : {x:bool|x=true} -> {y:bool|y/=x} -> {z:bool|?}
f13 = \x:bool. \y:bool. and x y
