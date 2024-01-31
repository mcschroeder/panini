not : (x:bool) -> {y:bool|y = true <=> x = false}

f21 : {x:bool|x=false} -> bool
f21 = \x:bool. not x

f53 : {x:bool|x=false} -> {y:bool|true}
f53 = \x:bool. not x

f50 : {x:bool|x=false} -> {y:bool|?}
f50 = \x:bool. not x

f51 : {x:bool|x=false} -> {y:bool|y=true <=> x=false}
f51 = \x:bool. not x

f52 : {x:bool|x=false} -> {y:bool|y=true}
f52 = \x:bool. not x

f54 : {x:bool|x=false} -> {y:bool|y=true /\ x=false}
f54 = \x:bool. not x

f42 : {x:bool|?} -> {y:bool|y=true}
f42 = \x:bool. not x

f44 : {x:bool|?} -> {y:bool|y=true /\ x=false}
f44 = \x:bool. not x


f51b : {x:bool|x=false} -> {y:bool|y=~x}
f51b = \x:bool. not x

f54b : {x:bool|x=false} -> {y:bool|y=true /\ y=~x}
f54b = \x:bool. not x

f44b : {x:bool|?} -> {y:bool|y=true /\ y=~x}
f44b = \x:bool. not x
