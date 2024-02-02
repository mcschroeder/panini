and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f10 = and true true

f11 = and true false

f12 = and false true

f13 = and false false
