
f40 : {b:bool|?} -> {v:int|v=2}
f40 = \b:bool. if b then 1 else 2

f41 : {b:bool|?} -> {v:int|v = 2 \/ v = 3}
f41 = \b:bool. if b then 1 else 2

f42 : {b:bool|?} -> {v:int|v >= 2}
f42 = \b:bool. if b then 1 else 2
