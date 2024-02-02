not : (x:bool) -> {y:bool|y = ~x}

f80 : {x:bool|x=true} -> {y:bool|y=~x}
f80 = \x:bool. not x

f81 : {x:bool|x=true} -> {y:bool|y=false}
f81 = \x:bool. not x

f82 : {x:bool|x=true} -> {y:bool|y=false /\ y=~x}
f82 = \x:bool. not x


