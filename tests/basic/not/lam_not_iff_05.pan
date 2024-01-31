not : (x:bool) -> {y:bool|y = true <=> x = false}

f31 : bool -> {y:bool|y=true}
f31 = \x:bool. not x

f32 : bool -> {y:bool|y=false}
f32 = \x:bool. not x

f51 : {x:bool|x=true} -> {y:bool|y=true}
f51 = \x:bool. not x

f62 : {x:bool|x=false} -> {y:bool|y=false}
f62 = \x:bool. not x

f71 : {x:bool|true} -> {y:bool|y=true}
f71 = \x:bool. not x

f72 : {x:bool|true} -> {y:bool|y=false}
f72 = \x:bool. not x

f99 : {x:bool|true} -> {y:bool|y=x}
f99 = \x:bool. not x
