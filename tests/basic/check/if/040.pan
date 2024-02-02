
f40 : {v:int|v<1}
f40 = if true then 1 else 2

f41 : {v:int|v=2}
f41 = if true then 1 else 2

f42 : {v:int|v>2}
f42 = if false then 1 else 2

f43 : {v:int|v=1}
f43 = if false then 1 else 2

f44 : {v:int|v<1}
f44 = let b = true in if b then 1 else 2

f45 : {v:int|v=2}
f45 = let b = true in if b then 1 else 2

f46 : {v:int|v>2}
f46 = let b = false in if b then 1 else 2

f47 : {v:int|v=1}
f47 = let b = false in if b then 1 else 2
