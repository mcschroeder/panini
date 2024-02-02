not : (x:bool) -> {y:bool|y = true <=> x = false}

f70 : {x:bool|x=true} -> bool
f70 = \x:bool. not x

f71 : {x:bool|x=true} -> {y:bool|true}
f71 = \x:bool. not x
