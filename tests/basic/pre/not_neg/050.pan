not : (x:bool) -> {y:bool|y = ~x}

f50 : {x:bool|?} -> {y:bool|y=true}
f50 = \x:bool. not x

f51 : {x:bool|?} -> {y:bool|y=true /\ y=~x}
f51 = \x:bool. not x

