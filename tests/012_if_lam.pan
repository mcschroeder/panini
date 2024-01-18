f0 = \b:bool. if b then 1 else 2

f1 : bool -> int
f1 = \b:bool. if b then 1 else 2

f2 : {b:bool|?} -> int
f2 = \b:bool. if b then 1 else 2

f3 : bool -> {n:int|?}
f3 = \b:bool. if b then 1 else 2

f4 : {b:bool|?} -> {n:int|?}
f4 = \b:bool. if b then 1 else 2

f5 : {b:bool|b=true} -> {n:int|?}
f5 = \b:bool. if b then 1 else 2

f6 : {b:bool|b=false} -> {n:int|?}
f6 = \b:bool. if b then 1 else 2

f7 : {b:bool|false} -> {n:int|?}
f7 = \b:bool. if b then 1 else 2

f8 : {b:bool|?} -> {n:int|n=1}
f8 = \b:bool. if b then 1 else 2

f9 : {b:bool|?} -> {n:int|n=2}
f9 = \b:bool. if b then 1 else 2

f10 : {b:bool|?} -> {n:int|n=3}
f10 = \b:bool. if b then 1 else 2

f11 : {b:bool|?} -> {n:int|false}
f11 = \b:bool. if b then 1 else 2

f12 : {b:bool|b=true} -> {n:int|n=1}
f12 = \b:bool. if b then 1 else 2

f13 : {b:bool|b=false} -> {n:int|n=2}
f13 = \b:bool. if b then 1 else 2

f14 : {b:bool|b=true} -> {n:int|n=2}     -- expected error
f14 = \b:bool. if b then 1 else 2

f15 : {b:bool|b=false} -> {n:int|n=1}    -- expected error
f15 = \b:bool. if b then 1 else 2
