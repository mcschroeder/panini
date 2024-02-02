id : (i:ℤ) → { j:ℤ | j = i }

f60 : {a:int|true} -> {b:int|b=a}
f60 = \x:int. id x
