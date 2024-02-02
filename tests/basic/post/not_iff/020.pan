not : (x:bool) -> {y:bool|y = true <=> x = false}

f20 : {x:bool|x=true} -> {y:bool|?}
f20 = \x:bool. not x

f21 : {x:bool|x=false} -> {y:bool|?}
f21 = \x:bool. not x

