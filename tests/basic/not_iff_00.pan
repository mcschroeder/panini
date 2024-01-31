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

f6 : {b:bool|b=false}
f6 = not true

f7 : {b:bool|b=true}
f7 = not false

f8 : {b:bool|false}
f8 = not true

f9 : {b:bool|false}
f9 = not false
