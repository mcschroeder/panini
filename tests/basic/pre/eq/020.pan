eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f20 : {n:int|?} -> {v:bool|v=false}
f20 = \x:int. eq x 1

f21 : {n:int|?} -> {v:bool|v=false}
f21 = \x:int. eq 1 x

