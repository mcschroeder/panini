
f20 : {v:int|v > 0}
f20 = if true then 1 else 2

f21 : {v:int|v > 0}
f21 = if false then 1 else 2

f22 : {v:int|v > 0}
f22 = let b = true in if b then 1 else 2

f23 : {v:int|v > 0}
f23 = let b = false in if b then 1 else 2
