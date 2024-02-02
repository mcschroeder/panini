id : (i:ℤ) → { j:ℤ | j = i }

f80 : {a:int|a=1} -> {b:int|b>=1}
f80 = \x:int. id x

f81 : {a:int|a>0} -> {b:int|b>=1}
f81 = \x:int. id x
