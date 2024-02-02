not : (x:bool) -> {y:bool|y = ~x}

f10 : {x:bool|?} -> bool
f10 = \x:bool. not x

f11 : {x:bool|?} -> {y:bool|true}
f11 = \x:bool. not x

