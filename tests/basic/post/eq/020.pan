eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f20 : {n:int|n=1} -> {v:bool|?}
f20 = \x:int. eq x 1

f21 : {n:int|n=1} -> {v:bool|?}
f21 = \x:int. eq 1 x

