id : (i:ℤ) → { j:ℤ | j = i }

f12 : {a:int|a=1} -> {b:int|?}
f12 = \x:int. id x

f22 : {a:int|?} -> {b:int|b=1}
f22 = \x:int. id x

f32 : {a:int|a=1} -> {b:int|b=1}
f32 = \x:int. id x

f33 : {a:int|a=1} -> {b:int|b=a}
f33 = \x:int. id x

f34 : {a:int|a=1} -> {b:int|b=1 /\ b=a}
f34 = \x:int. id x


