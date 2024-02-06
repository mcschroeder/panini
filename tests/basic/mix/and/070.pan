and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f70 : {x:bool|x=true} -> {y:bool|?} -> {z:bool|true}
f70 = \x:bool. \y:bool. and x y




f72 : {x:bool|true} -> {y:bool|?} -> {z:bool|true}
f72 = \x:bool. \y:bool. and x y

f73 : {x:bool|false} -> {y:bool|?} -> {z:bool|true}
f73 = \x:bool. \y:bool. and x y
