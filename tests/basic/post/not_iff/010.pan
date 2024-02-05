not : (x:bool) -> {y:bool|y = true <=> x = false}

f10 : bool -> {y:bool|?}
f10 = \x:bool. not x
