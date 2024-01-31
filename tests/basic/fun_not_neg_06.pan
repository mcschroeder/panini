not : (x:bool) -> {y:bool|y = ~x}

f34 : bool -> {y:bool|false}
f34 = \x:bool. not x

f54 : {x:bool|x=true} -> {y:bool|false}
f54 = \x:bool. not x

f64 : {x:bool|x=false} -> {y:bool|false}
f64 = \x:bool. not x

f74 : {x:bool|true} -> {y:bool|false}
f74 = \x:bool. not x