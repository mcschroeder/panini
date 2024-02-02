not : (x:bool) -> {y:bool|y = ~x}

f40 : {x:bool|?} -> {y:bool|false}
f40 = \x:bool. not x

f41 : {x:bool|?} -> {y:bool|y=x}
f41 = \x:bool. not x

