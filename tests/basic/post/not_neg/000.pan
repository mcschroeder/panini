not : (x:bool) -> {y:bool|y = ~x}

f00 : {b:bool|?}
f00 = not true

f01 : {b:bool|?}
f01 = not false
