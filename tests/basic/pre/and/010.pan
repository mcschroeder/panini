and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f10 : {x:bool|?} -> {y:bool|y=true} -> {z:bool|z=true}
f10 = \x:bool. \y:bool. and x y

f11 : {x:bool|?} -> {y:bool|y=false} -> {z:bool|z=true}
f11 = \x:bool. \y:bool. and x y

f12 : {x:bool|?} -> {y:bool|true} -> {z:bool|z=true}
f12 = \x:bool. \y:bool. and x y

f13 : {x:bool|?} -> {y:bool|false} -> {z:bool|z=true}
f13 = \x:bool. \y:bool. and x y

f14 : {x:bool|?} -> {y:bool|y=x} -> {z:bool|z=true}
f14 = \x:bool. \y:bool. and x y

f15 : {x:bool|?} -> {y:bool|y/=x} -> {z:bool|z=true}
f15 = \x:bool. \y:bool. and x y
