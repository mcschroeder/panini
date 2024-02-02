not : (x:bool) -> {y:bool|y = true <=> x = false}

f20 : {b:bool|?}
f20 = not true

f21 : {b:bool|?}
f21 = not false

