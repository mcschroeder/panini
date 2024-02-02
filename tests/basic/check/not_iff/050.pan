not : (x:bool) -> {y:bool|y = true <=> x = false}

f50 : {x:bool|true} -> {y:bool|y = ~x}
f50 = \x:bool. not x

f51 : {x:bool|true} -> {y:bool|y = true <=> x = false}
f51 = \x:bool. not x

f52 : {x:bool|true} -> {y:bool|y ≠ x}
f52 = \x:bool. not x
