not : (x:bool) -> {y:bool|y = ~x}

f40 : bool -> {y:bool|?}
f40 = \x:bool. not x

f41 : {x:bool|?} -> {y:bool|?}
f41 = \x:bool. not x

