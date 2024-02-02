and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f35 : {x:bool|true} -> {y:bool|false} -> {z:bool|?}
f35 = \x:bool. \y:bool. and x y
