not : (x:bool) -> {y:bool|y = true <=> x = false}

f60 : bool -> bool
f60 = \x:bool. not x

f61 : {x:bool|true} -> bool
f61 = \x:bool. not x

f62 : bool -> {y:bool|true}
f62 = \x:bool. not x

f63 : {x:bool|true} -> {y:bool|true}
f63 = \x:bool. not x

