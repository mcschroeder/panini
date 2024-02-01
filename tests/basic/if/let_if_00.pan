
f00 = let b = true in if b then 1 else 2

f10 : {v:int|?}
f10 = let b = true in if b then 1 else 2

f20 : {v:int|v=1}
f20 = let b = true in if b then 1 else 2


f01 = let b = false in if b then 1 else 2

f11 : {v:int|?}
f11 = let b = false in if b then 1 else 2

f21 : {v:int|v=2}
f21 = let b = false in if b then 1 else 2
