id : (i:ℤ) → { j:ℤ | j = i }

f14 : int -> {b:int|b=1}
f14 = \x:int. id x

f17 : {a:int|a=1} -> {b:int|b=2}
f17 = \x:int. id x

f18 : {a:int|a>2} -> {b:int|b=2}
f18 = \x:int. id x

f22 : {a:int|a=1} -> {b:int|false}
f22 = \x:int. id x
