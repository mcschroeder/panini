f0 = let b = true in if b then 1 else 2

f1 = let b = false in if b then 1 else 2

f2 : int
f2 = let b = true in if b then 1 else 2

f3 : int
f3 = let b = false in if b then 1 else 2

f4 : {n:int|?}
f4 = let b = true in if b then 1 else 2

f5 : {n:int|?}
f5 = let b = false in if b then 1 else 2

f6 : {n:int|n=1}
f6 = let b = true in if b then 1 else 2

f7 : {n:int|n=2}
f7 = let b = false in if b then 1 else 2

f8 : {n:int|n=2}                           -- expected error
f8 = let b = true in if b then 1 else 2

f9 : {n:int|n=1}                           -- expected error
f9 = let b = false in if b then 1 else 2

f10 : {n:int|n=3}                          -- expected error
f10 = let b = true in if b then 1 else 2

f11 : {n:int|n=3}                          -- expected error
f11 = let b = false in if b then 1 else 2
