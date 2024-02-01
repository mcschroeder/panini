id : (i:ℤ) → { j:ℤ | j = i }

f7 = \x:int. id x

f11 : {a:int|?} -> {b:int|?}
f11 = \x:int. id x

f21 : {a:int|true} -> {b:int|b=a}
f21 = \x:int. id x

f31 : {a:int|?} -> {b:int|b=a}
f31 = \x:int. id x

f41 : {a:int|true} -> {b:int|?}
f41 = \x:int. id x
