
f60 : bool -> int
f60 = \b:bool. if b then 1 else 2

f61 : {b:bool|true} -> {v:int|v=1 \/ v = 2}
f61 = \b:bool. if b then 1 else 2

f62 : {b:bool|true} -> {v:int|v>0}
f62 = \b:bool. if b then 1 else 2
