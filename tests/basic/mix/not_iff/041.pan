not : (x:bool) -> {y:bool|y = true <=> x = false}

f41 : {x:bool|?} -> {y:bool|?}
f41 = \x:bool. not x
