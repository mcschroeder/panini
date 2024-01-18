f0 = if true then 1 else 2

f1 = if false then 1 else 2

f2 : int
f2 = if true then 1 else 2

f3 : int
f3 = if false then 1 else 2

f4 : {n:int|?}
f4 = if true then 1 else 2

f5 : {n:int|?}
f5 = if false then 1 else 2

f6 : {n:int|n=1}
f6 = if true then 1 else 2

f7 : {n:int|n=2}
f7 = if false then 1 else 2

f8 : {n:int|n=2}               -- expected error
f8 = if true then 1 else 2

f9 : {n:int|n=1}               -- expected error
f9 = if false then 1 else 2

f10 : {n:int|n=3}              -- expected error
f10 = if true then 1 else 2

f11 : {n:int|n=3}              -- expected error
f11 = if false then 1 else 2
