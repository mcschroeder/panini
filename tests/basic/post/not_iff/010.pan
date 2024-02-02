not : (x:bool) -> {y:bool|y = true <=> x = false}

f10 : bool -> {y:bool|?}
f10 = \x:bool. not x

f11 : {x:bool|true} -> {y:bool|?}
f11 = \x:bool. not x

