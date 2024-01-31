not : (x:bool) -> {y:bool|y = true <=> x = false}

f24 : {x:bool|false} -> bool
f24 = \x:bool. not x

f80 : {x:bool|false} -> {y:bool|?}
f80 = \x:bool. not x

f81 : {x:bool|false} -> {y:bool|y=true}
f81 = \x:bool. not x

f82 : {x:bool|false} -> {y:bool|y=false}
f82 = \x:bool. not x

f83 : {x:bool|false} -> {y:bool|true}
f83 = \x:bool. not x

f84 : {x:bool|false} -> {y:bool|false}
f84 = \x:bool. not x

f44 : {x:bool|?} -> {y:bool|false}
f44 = \x:bool. not x

f41 : {x:bool|?} -> {y:bool|y=x}
f41 = \x:bool. not x
