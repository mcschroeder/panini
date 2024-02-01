eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f00 = \x:int. eq x 1

f10 = \x:int. eq 1 x


f01 : {n:int|?} -> {v:bool|?}
f01 = \x:int. eq x 1

f11 : {n:int|?} -> {v:bool|?}
f11 = \x:int. eq 1 x


f02 : {n:int|true} -> {v:bool|?}
f02 = \x:int. eq x 1

f12 : {n:int|true} -> {v:bool|?}
f12 = \x:int. eq 1 x


f03 : {n:int|?} -> {v:bool|v = true <=> n = 1}
f03 = \x:int. eq x 1

f13 : {n:int|?} -> {v:bool|v = true <=> n = 1}
f13 = \x:int. eq 1 x


f04 : {n:int|true} -> {v:bool|v = true <=> n = 1}
f04 = \x:int. eq x 1

f14 : {n:int|true} -> {v:bool|v = true <=> n = 1}
f14 = \x:int. eq 1 x
