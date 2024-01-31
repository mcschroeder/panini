not : (x:bool) -> {y:bool|y = true <=> x = false}

f21 : {x:bool|x=true} -> bool
f21 = \x:bool. not x

f53 : {x:bool|x=true} -> {y:bool|true}
f53 = \x:bool. not x

f50 : {x:bool|x=true} -> {y:bool|?}
f50 = \x:bool. not x

f51 : {x:bool|x=true} -> {y:bool|y=true <=> x=false}
f51 = \x:bool. not x

f52 : {x:bool|x=true} -> {y:bool|y=false}
f52 = \x:bool. not x

f54 : {x:bool|x=true} -> {y:bool|y=false /\ x=true}
f54 = \x:bool. not x

f42 : {x:bool|?} -> {y:bool|y=false}
f42 = \x:bool. not x

f44 : {x:bool|?} -> {y:bool|y=false /\ x=true}
f44 = \x:bool. not x


f51b : {x:bool|x=true} -> {y:bool|y=~x}
f51b = \x:bool. not x

f54b : {x:bool|x=true} -> {y:bool|y=false /\ y=~x}
f54b = \x:bool. not x

f44b : {x:bool|?} -> {y:bool|y=false /\ y=~x}
f44b = \x:bool. not x
