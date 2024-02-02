not : (x:bool) -> {y:bool|y = ~x}

f30 : {x:bool|false} -> {y:bool|?}
f30 = \x:bool. not x

