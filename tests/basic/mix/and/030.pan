and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f30 : {x:bool|?} -> {y:bool|y=true} -> {z:bool|?}
f30 = \x:bool. \y:bool. and x y

f31 : {x:bool|?} -> {y:bool|y=false} -> {z:bool|?}
f31 = \x:bool. \y:bool. and x y

f32 : {x:bool|?} -> {y:bool|y=x} -> {z:bool|?}
f32 = \x:bool. \y:bool. and x y




f34 : {x:bool|?} -> {y:bool|true} -> {z:bool|?}
f34 = \x:bool. \y:bool. and x y
