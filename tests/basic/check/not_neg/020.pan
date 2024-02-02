not : (x:bool) -> {y:bool|y = ~x}

f20 : {b:bool|b=false}
f20 = not true

f21 : {b:bool|b=true}
f21 = not false
