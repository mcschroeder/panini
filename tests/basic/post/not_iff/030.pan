not : (x:bool) -> {y:bool|y = true <=> x = false}

f30 : {x:bool|false} -> {y:bool|?}
f30 = \x:bool. not x

