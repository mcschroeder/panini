eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f80 : {n:int|n=1} -> {v:bool|v = true}
f80 = \x:int. eq x 1

f81 : {n:int|n=1} -> {v:bool|v = true}
f81 = \x:int. eq 1 x

