and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f20 : {z:bool|?}
f20 = and true true

f21 : {z:bool|?}
f21 = and true false

f22 : {z:bool|?}
f22 = and false true

f23 : {z:bool|?}
f23 = and false false

