and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f60 : {x:bool|?} -> {y:bool|?} -> {z:bool|?}
f60 = and

f61 : {x:bool|?} -> {y:bool|?} -> {z:bool|?}
f61 = \x:bool. \y:bool. and x y
