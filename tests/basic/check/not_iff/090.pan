not : (x:bool) -> {y:bool|y = true <=> x = false}

f90 : {x:bool|x=false} -> bool
f90 = \x:bool. not x

f91 : {x:bool|x=false} -> {y:bool|true}
f91 = \x:bool. not x

f92 : {x:bool|x=false} -> {y:bool|y=~x}
f92 = \x:bool. not x

f93 : {x:bool|x=false} -> {y:bool|y=true}
f93 = \x:bool. not x

f94 : {x:bool|x=false} -> {y:bool|y=true /\ y=~x}
f94 = \x:bool. not x
