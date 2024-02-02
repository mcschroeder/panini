
f20 : {b:bool|?} -> {v:int|true}
f20 = \b:bool. if b then 1 else 2

f21 : {b:bool|?} -> {v:int|v=1 \/ v = 2}
f21 = \b:bool. if b then 1 else 2

f22 : {b:bool|?} -> {v:int|v=1 \/ v = 2 \/ v = 3}
f22 = \b:bool. if b then 1 else 2

f23 : {b:bool|?} -> {v:int|v>0}
f23 = \b:bool. if b then 1 else 2
