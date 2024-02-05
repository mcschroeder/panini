f60 : {x:bool|?} -> {v:int|v=1}
f60 = \x:bool. let y = x in if y then 1 else 2

f61 : {x:bool|?} -> {v:int|v=2}
f61 = \x:bool. let y = x in if y then 1 else 2
