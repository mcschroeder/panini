not : (x:bool) -> {y:bool|y = true <=> x = false}

f8 : {b:bool|false}
f8 = not true

f9 : {b:bool|false}
f9 = not false
