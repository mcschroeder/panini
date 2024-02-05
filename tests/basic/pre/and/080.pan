and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f80 : {x:bool|x=true} -> {y:bool|?} -> {z:bool|false}
f80 = \x:bool. \y:bool. and x y

f81 : {x:bool|x=false} -> {y:bool|?} -> {z:bool|false}
f81 = \x:bool. \y:bool. and x y

f82 : {x:bool|true} -> {y:bool|?} -> {z:bool|false}
f82 = \x:bool. \y:bool. and x y

f83 : {x:bool|false} -> {y:bool|?} -> {z:bool|false}
f83 = \x:bool. \y:bool. and x y
