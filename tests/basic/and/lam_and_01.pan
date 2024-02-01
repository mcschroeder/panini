and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f122 : {x:bool|x=true} -> {y:bool|?} -> {z:bool|?}
f122 = \x:bool. \y:bool. and x y

f022 : {x:bool|x=false} -> {y:bool|?} -> {z:bool|?}
f022 = \x:bool. \y:bool. and x y

f212 : {x:bool|?} -> {y:bool|y=true} -> {z:bool|?}
f212 = \x:bool. \y:bool. and x y

f202 : {x:bool|?} -> {y:bool|y=false} -> {z:bool|?}
f202 = \x:bool. \y:bool. and x y

f221 : {x:bool|?} -> {y:bool|?} -> {z:bool|z=true}
f221 = \x:bool. \y:bool. and x y

f220 : {x:bool|?} -> {y:bool|?} -> {z:bool|z=false}
f220 = \x:bool. \y:bool. and x y

f121 : {x:bool|x=true} -> {y:bool|?} -> {z:bool|z=true}
f121 = \x:bool. \y:bool. and x y

f120 : {x:bool|x=true} -> {y:bool|?} -> {z:bool|z=false}
f120 = \x:bool. \y:bool. and x y

f021 : {x:bool|x=false} -> {y:bool|?} -> {z:bool|z=true}
f021 = \x:bool. \y:bool. and x y

f020 : {x:bool|x=false} -> {y:bool|?} -> {z:bool|z=false}
f020 = \x:bool. \y:bool. and x y

f211 : {x:bool|?} -> {y:bool|y=true} -> {z:bool|z=true}
f211 = \x:bool. \y:bool. and x y

f210 : {x:bool|?} -> {y:bool|y=true} -> {z:bool|z=false}
f210 = \x:bool. \y:bool. and x y

f201 : {x:bool|?} -> {y:bool|y=false} -> {z:bool|z=true}
f201 = \x:bool. \y:bool. and x y

f200 : {x:bool|?} -> {y:bool|y=false} -> {z:bool|z=false}
f200 = \x:bool. \y:bool. and x y
