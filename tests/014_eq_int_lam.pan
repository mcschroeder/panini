eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f0 = \x:int. eq x 1

f1 : int -> bool
f1 = \x:int. eq x 1

f2 : {n:int|?} -> bool
f2 = \x:int. eq x 1

f3 : int -> {v:bool|?}
f3 = \x:int. eq x 1

f4 : {n:int|?} -> {v:bool|?}
f4 = \x:int. eq x 1

f5 : {n:int|n=1} -> {v:bool|?}
f5 = \x:int. eq x 1

f6 : {n:int|?} -> {v:bool|v=true}
f6 = \x:int. eq x 1

f7 : {n:int|?} -> {v:bool|v=false}
f7 = \x:int. eq x 1

f8 : {n:int|n=2} -> {v:bool|?}
f8 = \x:int. eq x 1

f9 : {n:int|n=1} -> {v:bool|v=true}
f9 = \x:int. eq x 1

f10 : {n:int|n=1} -> {v:bool|v=false}      -- expected error
f10 = \x:int. eq x 1

f11 : {n:int|n=2} -> {v:bool|v=true}       -- expected error
f11 = \x:int. eq x 1

f12 : {n:int|n=2} -> {v:bool|v=false}
f12 = \x:int. eq x 1

f13 : {n:int| n >= 1} -> {v:bool|?}
f13 = \x:int. eq x 1

f14 : {n:int| n > 1} -> {v:bool|?}
f14 = \x:int. eq x 1

f15 : {n:int| n > 1} -> {v:bool|v=false}
f15 = \x:int. eq x 1
