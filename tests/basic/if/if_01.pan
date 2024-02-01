
f30 : int
f30 = if true then 1 else 2

f40 : {v:int|v > 0}
f40 = if true then 1 else 2

f50 : {v:int|v <= 1}
f50 = if true then 1 else 2


f31 : int
f31 = if false then 1 else 2

f41 : {v:int|v > 0}
f41 = if false then 1 else 2

f51 : {v:int|v >= 2}
f51 = if false then 1 else 2
