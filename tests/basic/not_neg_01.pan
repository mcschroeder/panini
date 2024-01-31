not : (x:bool) -> {y:bool|y = ~x}

f6 : {b:bool|b=true}
f6 = not true

f7 : {b:bool|b=false}
f7 = not false
