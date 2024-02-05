f70 : {x:bool|?} -> {v:int|v=1}
f70 = \x:bool. if x then let b = true in if b then 1 else 3 else 2

f71 : {x:bool|?} -> {v:int|v=2}
f71 = \x:bool. if x then let b = true in if b then 1 else 3 else 2

f72 : {x:bool|?} -> {v:int|v=3}
f72 = \x:bool. if x then let b = true in if b then 1 else 3 else 2

f73 : {x:bool|?} -> {v:int|v=4}
f73 = \x:bool. if x then let b = true in if b then 1 else 3 else 2
