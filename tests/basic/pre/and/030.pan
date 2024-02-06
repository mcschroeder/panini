and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f30 : {x:bool|?} -> {y:bool|y=true} -> {z:bool|true}
f30 = \x:bool. \y:bool. and x y




f32 : {x:bool|?} -> {y:bool|true} -> {z:bool|true}
f32 = \x:bool. \y:bool. and x y

f33 : {x:bool|?} -> {y:bool|false} -> {z:bool|true}
f33 = \x:bool. \y:bool. and x y

f34 : {x:bool|?} -> {y:bool|y=x} -> {z:bool|true}
f34 = \x:bool. \y:bool. and x y

f35 : {x:bool|?} -> {y:bool|y/=x} -> {z:bool|true}
f35 = \x:bool. \y:bool. and x y
