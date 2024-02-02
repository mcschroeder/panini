
f50 : {b:bool|?} -> {v:int|false}
f50 = \b:bool. if b then 1 else 2

f51 : {b:bool|?} -> {v:int|v=3}
f51 = \b:bool. if b then 1 else 2

f52 : {b:bool|?} -> {v:int|v>2}
f52 = \b:bool. if b then 1 else 2
