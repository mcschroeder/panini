
f20 : {b:bool|?} -> {v:int|v=1}
f20 = \b:bool. if b then 1 else 2

f30 : {b:bool|b=true} -> {v:int|?}
f30 = \b:bool. if b then 1 else 2


f21 : {b:bool|?} -> {v:int|v=2}
f21 = \b:bool. if b then 1 else 2

f22 : {b:bool|?} -> {v:int|v=3}
f22 = \b:bool. if b then 1 else 2


f31 : {b:bool|b=false} -> {v:int|?}
f31 = \b:bool. if b then 1 else 2
