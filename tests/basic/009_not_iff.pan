not : (x:bool) -> {y:bool|y = true <=> x = false}

f0 = not true

f1 = not false

f2 : bool
f2 = not true

f3 : bool
f3 = not false

f4 : {b:bool|?}
f4 = not true

f5 : {b:bool|?}
f5 = not false