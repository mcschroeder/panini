and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f40 : {x:bool|?} -> {y:bool|y=true} -> {z:bool|false}
f40 = \x:bool. \y:bool. and x y

f41 : {x:bool|?} -> {y:bool|y=false} -> {z:bool|false}
f41 = \x:bool. \y:bool. and x y

f42 : {x:bool|?} -> {y:bool|true} -> {z:bool|false}
f42 = \x:bool. \y:bool. and x y

f43 : {x:bool|?} -> {y:bool|false} -> {z:bool|false}
f43 = \x:bool. \y:bool. and x y

f44 : {x:bool|?} -> {y:bool|y=x} -> {z:bool|false}
f44 = \x:bool. \y:bool. and x y

f45 : {x:bool|?} -> {y:bool|y/=x} -> {z:bool|false}
f45 = \x:bool. \y:bool. and x y