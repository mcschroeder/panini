not : (x:bool) -> {y:bool|y = ~x}

f21 : {x:bool|x=false} -> bool
f21 = \x:bool. not x

f63 : {x:bool|x=false} -> {y:bool|true}
f63 = \x:bool. not x

f50 : {x:bool|x=false} -> {y:bool|?}
f50 = \x:bool. not x

f51 : {x:bool|x=false} -> {y:bool|y=~x}
f51 = \x:bool. not x

f52 : {x:bool|x=false} -> {y:bool|y=true}
f52 = \x:bool. not x

f54 : {x:bool|x=false} -> {y:bool|y=true /\ y=~x}
f54 = \x:bool. not x

f41 : {x:bool|?} -> {y:bool|y=true}
f41 = \x:bool. not x

f44 : {x:bool|?} -> {y:bool|y=true /\ y=~x}
f44 = \x:bool. not x


