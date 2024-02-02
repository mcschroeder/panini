and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f63 : {x:bool|?} -> {y:bool|y/=x} -> {z:bool|?}
f63 = \x:bool. \y:bool. and x y