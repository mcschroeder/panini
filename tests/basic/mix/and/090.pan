and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f90 : {x:bool|?} -> {y:bool|?} -> {z:bool|z=true}
f90 = \x:bool. \y:bool. and x y

f91 : {x:bool|?} -> {y:bool|?} -> {z:bool|z=false}
f91 = \x:bool. \y:bool. and x y

f92 : {x:bool|?} -> {y:bool|?} -> {z:bool|true}
f92 = \x:bool. \y:bool. and x y

f93 : {x:bool|?} -> {y:bool|?} -> {z:bool|false}
f93 = \x:bool. \y:bool. and x y
