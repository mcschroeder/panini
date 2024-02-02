eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f30 : {n:int|n/=1} -> {v:bool|?}
f30 = \x:int. eq x 1

f31 : {n:int|n/=1} -> {v:bool|?}
f31 = \x:int. eq 1 x
