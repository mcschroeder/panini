not : (x:bool) -> {y:bool|y = true <=> x = false}

f120 : bool -> {y:bool|false}
f120 = \x:bool. not x

f121 : {x:bool|x=true} -> {y:bool|false}
f121 = \x:bool. not x

f122 : {x:bool|x=false} -> {y:bool|false}
f122 = \x:bool. not x

f123 : {x:bool|true} -> {y:bool|false}
f123 = \x:bool. not x

