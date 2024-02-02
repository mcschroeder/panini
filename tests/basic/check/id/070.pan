id : (i:ℤ) → { j:ℤ | j = i }

f70 : {a:int|a=1} -> {b:int|b=1}
f70 = \x:int. id x

f71 : {a:int|a=1} -> {b:int|b=a}
f71 = \x:int. id x

f72 : {a:int|a=1} -> {b:int|b=1 /\ b=a}
f72 = \x:int. id x

