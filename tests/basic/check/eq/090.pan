eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f90 : {n:int|n/=1} -> {v:bool|v = false}
f90 = \x:int. eq x 1

f91 : {n:int|n/=1} -> {v:bool|v = false}
f91 = \x:int. eq 1 x


