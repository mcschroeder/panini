
f30 : int
f30 = let b = true in if b then 1 else 2

f40 : {v:int|v > 0}
f40 = let b = true in if b then 1 else 2

f50 : {v:int|v <= 1}
f50 = let b = true in if b then 1 else 2


f31 : int
f31 = let b = false in if b then 1 else 2

f41 : {v:int|v > 0}
f41 = let b = false in if b then 1 else 2

f51 : {v:int|v >= 2}
f51 = let b = false in if b then 1 else 2
