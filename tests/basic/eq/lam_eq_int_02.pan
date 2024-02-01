eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f01 : {n:int|n/=1} -> {v:bool|?}
f01 = \x:int. eq x 1

f11 : {n:int|n/=1} -> {v:bool|?}
f11 = \x:int. eq 1 x


f02 : {n:int|?} -> {v:bool|v=false}
f02 = \x:int. eq x 1

f12 : {n:int|?} -> {v:bool|v=false}
f12 = \x:int. eq 1 x


f03 : {n:int|n/=1} -> {v:bool|v = false}
f03 = \x:int. eq x 1

f13 : {n:int|n/=1} -> {v:bool|v = false}
f13 = \x:int. eq 1 x
