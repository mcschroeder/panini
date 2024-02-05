and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f60 : {x:bool|x=true} -> {y:bool|?} -> {z:bool|z=false}
f60 = \x:bool. \y:bool. and x y

f61 : {x:bool|x=false} -> {y:bool|?} -> {z:bool|z=false}
f61 = \x:bool. \y:bool. and x y

f62 : {x:bool|true} -> {y:bool|?} -> {z:bool|z=false}
f62 = \x:bool. \y:bool. and x y

f63 : {x:bool|false} -> {y:bool|?} -> {z:bool|z=false}
f63 = \x:bool. \y:bool. and x y
