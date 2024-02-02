not : (x:bool) -> {y:bool|y = ~x}

f30 : {x:bool|?} -> {y:bool|y=false}
f30 = \x:bool. not x

f31 : {x:bool|?} -> {y:bool|y=false /\ y=~x}
f31 = \x:bool. not x

