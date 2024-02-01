and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f00 = and

f01 = \x:bool. \y:bool. and x y


f10 : {x:bool|?} -> {y:bool|?} -> {z:bool|?}
f10 = and

f11 : {x:bool|?} -> {y:bool|?} -> {z:bool|?}
f11 = \x:bool. \y:bool. and x y


f20 : x:bool -> y:bool -> {z:bool|z = true <=> (x = true /\ y = true)}
f20 = and

f21 : x:bool -> y:bool -> {z:bool|z = true <=> (x = true /\ y = true)}
f21 = \x:bool. \y:bool. and x y
