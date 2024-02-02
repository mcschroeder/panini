id : (i:ℤ) → { j:ℤ | j = i }

f100 : int -> {b:int|b=1}
f100 = \x:int. id x

f101 : {a:int|a=1} -> {b:int|b=2}
f101 = \x:int. id x

f102 : {a:int|a>2} -> {b:int|b=2}
f102 = \x:int. id x

f103 : {a:int|a=1} -> {b:int|false}
f103 = \x:int. id x

