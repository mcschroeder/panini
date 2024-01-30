not : (x:bool) -> {y:bool|y = ~x}

f00 = \x:bool. not x

----------------------------------------

f10 : bool -> bool
f10 = \x:bool. not x

----------------------------------------

f20 : {x:bool|?} -> bool
f20 = \x:bool. not x

f21 : {x:bool|x=true} -> bool
f21 = \x:bool. not x

f22 : {x:bool|x=false} -> bool
f22 = \x:bool. not x

f23 : {x:bool|true} -> bool
f23 = \x:bool. not x

f24 : {x:bool|false} -> bool
f24 = \x:bool. not x

----------------------------------------

f30 : bool -> {y:bool|?}
f30 = \x:bool. not x

f31 : bool -> {y:bool|y=true}
f31 = \x:bool. not x

f32 : bool -> {y:bool|y=false}
f32 = \x:bool. not x

f33 : bool -> {y:bool|true}
f33 = \x:bool. not x

f34 : bool -> {y:bool|false}
f34 = \x:bool. not x

----------------------------------------

f40 : {x:bool|?} -> {y:bool|?}
f40 = \x:bool. not x

f41 : {x:bool|?} -> {y:bool|y=true}
f41 = \x:bool. not x

f42 : {x:bool|?} -> {y:bool|y=false}
f42 = \x:bool. not x

f43 : {x:bool|?} -> {y:bool|true}
f43 = \x:bool. not x

f44 : {x:bool|?} -> {y:bool|false}
f44 = \x:bool. not x

----------------------------------------

f50 : {x:bool|x=true} -> {y:bool|?}
f50 = \x:bool. not x

f51 : {x:bool|x=true} -> {y:bool|y=true}
f51 = \x:bool. not x

f52 : {x:bool|x=true} -> {y:bool|y=false}
f52 = \x:bool. not x

f53 : {x:bool|x=true} -> {y:bool|true}
f53 = \x:bool. not x

f54 : {x:bool|x=true} -> {y:bool|false}
f54 = \x:bool. not x

----------------------------------------

f60 : {x:bool|x=false} -> {y:bool|?}
f60 = \x:bool. not x

f61 : {x:bool|x=false} -> {y:bool|y=true}
f61 = \x:bool. not x

f62 : {x:bool|x=false} -> {y:bool|y=false}
f62 = \x:bool. not x

f63 : {x:bool|x=false} -> {y:bool|true}
f63 = \x:bool. not x

f64 : {x:bool|x=false} -> {y:bool|false}
f64 = \x:bool. not x

----------------------------------------

f70 : {x:bool|true} -> {y:bool|?}
f70 = \x:bool. not x

f71 : {x:bool|true} -> {y:bool|y=true}
f71 = \x:bool. not x

f72 : {x:bool|true} -> {y:bool|y=false}
f72 = \x:bool. not x

f73 : {x:bool|true} -> {y:bool|true}
f73 = \x:bool. not x

f74 : {x:bool|true} -> {y:bool|false}
f74 = \x:bool. not x

----------------------------------------

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
