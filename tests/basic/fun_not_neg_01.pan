not : (x:bool) -> {y:bool|y = ~x}

f10 : bool -> bool
f10 = \x:bool. not x

f20 : {x:bool|?} -> bool
f20 = \x:bool. not x

f23 : {x:bool|true} -> bool
f23 = \x:bool. not x

f33 : bool -> {y:bool|true}
f33 = \x:bool. not x

f43 : {x:bool|?} -> {y:bool|true}
f43 = \x:bool. not x

f73 : {x:bool|true} -> {y:bool|true}
f73 = \x:bool. not x
