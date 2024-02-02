and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f50 = and

f51 = \x:bool. \y:bool. and x y
