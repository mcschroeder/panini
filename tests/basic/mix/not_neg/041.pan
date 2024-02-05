not : (x:bool) -> {y:bool|y = ~x}

f41 : {x:bool|?} -> {y:bool|?}
f41 = \x:bool. not x
