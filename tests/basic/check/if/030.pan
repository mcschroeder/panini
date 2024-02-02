
f30 : {v:int|v=1}
f30 = if true then 1 else 2

f31 : {v:int|v=2}
f31 = if false then 1 else 2

f32 : {v:int|v=1}
f32 = let b = true in if b then 1 else 2

f33 : {v:int|v=2}
f33 = let b = false in if b then 1 else 2
