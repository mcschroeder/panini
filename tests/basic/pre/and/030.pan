and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f30 : {x:bool|?} -> {y:bool|y=true} -> {z:bool|z=true}
f30 = \x:bool. \y:bool. and x y

f31 : {x:bool|?} -> {y:bool|y=true} -> {z:bool|z=false}
f31 = \x:bool. \y:bool. and x y

f33 : {x:bool|?} -> {y:bool|y=false} -> {z:bool|z=true}
f33 = \x:bool. \y:bool. and x y

f34 : {x:bool|?} -> {y:bool|y=false} -> {z:bool|z=false}
f34 = \x:bool. \y:bool. and x y
