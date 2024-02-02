id : (i:ℤ) → { j:ℤ | j = i }

f10 : {a:int|?} -> {b:int|b=a}
f10 = \x:int. id x


