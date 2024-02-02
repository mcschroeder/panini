
f30 : {b:bool|?} -> {v:int|v=1}
f30 = \b:bool. if b then 1 else 2

f31 : {b:bool|?} -> {v:int|v=1 \/ v = 3}
f31 = \b:bool. if b then 1 else 2

f32 : {b:bool|?} -> {v:int|v<=1}
f32 = \b:bool. if b then 1 else 2
