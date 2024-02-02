not : (x:bool) -> {y:bool|y = ~x}

f20 : {x:bool|?} -> {y:bool|y = ~x}
f20 = \x:bool. not x

f21 : {x:bool|?} -> {y:bool|y = true <=> x = false}
f21 = \x:bool. not x
