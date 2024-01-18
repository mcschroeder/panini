id : (i:ℤ) → { j:ℤ | j = i }

f0 = id 0

f1 : int
f1 = id 0

f2 : {n:int|?}
f2 = id 0

f3 : {n:int|?}
f3 = id 1

f4 : {n:int|n=0}
f4 = id 0

f5 : {n:int|n=1}
f5 = id 1

f6 : {n:int|n=1}     -- expected error
f6 = id 0

f7 = \x:int. id x

f8 : int -> int
f8 = \x:int. id x

f9 : {a:int|?} -> int
f9 = \x:int. id x

f10 : int -> {b:int|?}
f10 = \x:int. id x

f11 : {a:int|?} -> {b:int|?}
f11 = \x:int. id x

f12 : {a:int|a=1} -> int
f12 = \x:int. id x

f13 : {a:int|a=1} -> {b:int|?}
f13 = \x:int. id x

f14 : int -> {b:int|b=1}     -- expected error
f14 = \x:int. id x

f15 : {a:int|?} -> {b:int|b=1}
f15 = \x:int. id x

f16 : {a:int|a=1} -> {b:int|b=1}
f16 = \x:int. id x

f17 : {a:int|a=1} -> {b:int|b=2}     -- expected error
f17 = \x:int. id x

f18 : {a:int|a>5} -> {b:int|b>1}
f18 = \x:int. id x

f19 : {a:int|a>5} -> {b:int|?}
f19 = \x:int. id x

f20 : {a:int|?} -> {b:int|b>1}
f20 = \x:int. id x
