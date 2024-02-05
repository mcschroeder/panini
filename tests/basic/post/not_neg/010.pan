not : (x:bool) -> {y:bool|y = ~x}

f10 : bool -> {y:bool|?}
f10 = \x:bool. not x
