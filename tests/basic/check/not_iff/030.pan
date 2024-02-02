not : (x:bool) -> {y:bool|y = true <=> x = false}

f30 : {b:bool|b=true}
f30 = not true

f31 : {b:bool|b=false}
f31 = not false
