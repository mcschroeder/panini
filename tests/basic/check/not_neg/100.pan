not : (x:bool) -> {y:bool|y = ~x}

f100 : {x:bool|false} -> bool
f100 = \x:bool. not x

f101 : {x:bool|false} -> {y:bool|y=true}
f101 = \x:bool. not x

f102 : {x:bool|false} -> {y:bool|y=false}
f102 = \x:bool. not x

f103 : {x:bool|false} -> {y:bool|true}
f103 = \x:bool. not x

f104 : {x:bool|false} -> {y:bool|false}
f104 = \x:bool. not x

