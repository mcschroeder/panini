id : (i:ℤ) → { j:ℤ | j = i }

f42 : {a:int|a=1} -> {b:int|b>=1}
f42 = \x:int. id x

f43 : {a:int|?} -> {b:int|b>=1}
f43 = \x:int. id x

f44 : {a:int|a>0} -> {b:int|b>=1}
f44 = \x:int. id x

f45 : {a:int|a>0} -> {b:int|?}
f45 = \x:int. id x

f77 : int -> int
f77 = \x:int. id x
