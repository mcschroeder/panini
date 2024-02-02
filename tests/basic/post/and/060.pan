and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f60 : {x:bool|?} -> {y:bool|y=true} -> {z:bool|?}
f60 = \x:bool. \y:bool. and x y

f61 : {x:bool|?} -> {y:bool|y=false} -> {z:bool|?}
f61 = \x:bool. \y:bool. and x y

f62 : {x:bool|?} -> {y:bool|y=x} -> {z:bool|?}
f62 = \x:bool. \y:bool. and x y




f64 : {x:bool|?} -> {y:bool|true} -> {z:bool|?}
f64 = \x:bool. \y:bool. and x y


