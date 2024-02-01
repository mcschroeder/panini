and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f00 = and true true

f01 = and true false

f02 = and false true

f03 = and false false


f10 : {z:bool|?}
f10 = and true true

f11 : {z:bool|?}
f11 = and true false

f12 : {z:bool|?}
f12 = and false true

f13 : {z:bool|?}
f13 = and false false


f20 : {z:bool|z=true}
f20 = and true true

f21 : {z:bool|z=false}
f21 = and true false

f22 : {z:bool|z=false}
f22 = and false true

f23 : {z:bool|z=false}
f23 = and false false
