not : (x:bool) -> {y:bool|y = ~x}

f00 = not

f10 = \x:bool. not x

f30 : bool -> {y:bool|?}
f30 = \x:bool. not x

f40 : {x:bool|?} -> {y:bool|?}
f40 = \x:bool. not x

f41 : {x:bool|?} -> {y:bool|y = ~x}
f41 = \x:bool. not x

f70 : {x:bool|true} -> {y:bool|?}
f70 = \x:bool. not x

f71 : {x:bool|true} -> {y:bool|y = ~x}
f71 = \x:bool. not x
