and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f10 : {z:bool|z=true}
f10 = and true true

f11 : {z:bool|z=false}
f11 = and true false

f12 : {z:bool|z=false}
f12 = and false true

f13 : {z:bool|z=false}
f13 = and false false
