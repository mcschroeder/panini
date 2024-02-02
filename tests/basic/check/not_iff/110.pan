not : (x:bool) -> {y:bool|y = true <=> x = false}

f110 : bool -> {y:bool|y=true}
f110 = \x:bool. not x

f111 : bool -> {y:bool|y=false}
f111 = \x:bool. not x

f112 : {x:bool|x=true} -> {y:bool|y=true}
f112 = \x:bool. not x

f113 : {x:bool|x=false} -> {y:bool|y=false}
f113 = \x:bool. not x

f114 : {x:bool|true} -> {y:bool|y=true}
f114 = \x:bool. not x

f115 : {x:bool|true} -> {y:bool|y=false}
f115 = \x:bool. not x

f116 : {x:bool|true} -> {y:bool|y=x}
f116 = \x:bool. not x

