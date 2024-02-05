and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f10 : {x:bool|?} -> {y:bool|?} -> {z:bool|?}
f10 = and

f11 : {x:bool|?} -> {y:bool|?} -> {z:bool|?}
f11 = \x:bool. \y:bool. and x y
