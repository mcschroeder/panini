and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f00 : {z:bool|?}
f00 = and true true

f01 : {z:bool|?}
f01 = and true false

f02 : {z:bool|?}
f02 = and false true

f03 : {z:bool|?}
f03 = and false false

