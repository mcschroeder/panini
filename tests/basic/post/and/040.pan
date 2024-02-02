and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f40 : {x:bool|false} -> {y:bool|y=true} -> {z:bool|?}
f40 = \x:bool. \y:bool. and x y

f41 : {x:bool|false} -> {y:bool|y=false} -> {z:bool|?}
f41 = \x:bool. \y:bool. and x y

f42 : {x:bool|false} -> {y:bool|y=x} -> {z:bool|?}
f42 = \x:bool. \y:bool. and x y

f43 : {x:bool|false} -> {y:bool|y/=x} -> {z:bool|?}
f43 = \x:bool. \y:bool. and x y

f44 : {x:bool|false} -> {y:bool|true} -> {z:bool|?}
f44 = \x:bool. \y:bool. and x y

f45 : {x:bool|false} -> {y:bool|false} -> {z:bool|?}
f45 = \x:bool. \y:bool. and x y
