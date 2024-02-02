and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f20 : {x:bool|x=false} -> {y:bool|y=true} -> {z:bool|?}
f20 = \x:bool. \y:bool. and x y

f21 : {x:bool|x=false} -> {y:bool|y=false} -> {z:bool|?}
f21 = \x:bool. \y:bool. and x y

f22 : {x:bool|x=false} -> {y:bool|y=x} -> {z:bool|?}
f22 = \x:bool. \y:bool. and x y

f23 : {x:bool|x=false} -> {y:bool|y/=x} -> {z:bool|?}
f23 = \x:bool. \y:bool. and x y

f24 : {x:bool|x=false} -> {y:bool|true} -> {z:bool|?}
f24 = \x:bool. \y:bool. and x y
