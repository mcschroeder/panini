and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f20 : {x:bool|?} -> {y:bool|y=true} -> {z:bool|z=false}
f20 = \x:bool. \y:bool. and x y

f21 : {x:bool|?} -> {y:bool|y=false} -> {z:bool|z=false}
f21 = \x:bool. \y:bool. and x y

f22 : {x:bool|?} -> {y:bool|true} -> {z:bool|z=false}
f22 = \x:bool. \y:bool. and x y

f23 : {x:bool|?} -> {y:bool|false} -> {z:bool|z=false}
f23 = \x:bool. \y:bool. and x y

f24 : {x:bool|?} -> {y:bool|y=x} -> {z:bool|z=false}
f24 = \x:bool. \y:bool. and x y

f25 : {x:bool|?} -> {y:bool|y/=x} -> {z:bool|z=false}
f25 = \x:bool. \y:bool. and x y
