not : (x:bool) -> {y:bool|y = ~x}

f11 : {x:bool|true} -> {y:bool|?}
f11 = \x:bool. not x
