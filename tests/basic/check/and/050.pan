and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f50 : x:bool -> y:bool -> {z:bool|z = true <=> (x = true /\ y = true)}
f50 = and

f51 : x:bool -> y:bool -> {z:bool|z = true <=> (x = true /\ y = true)}
f51 = \x:bool. \y:bool. and x y
