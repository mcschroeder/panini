eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f70 : {n:int|true} -> {v:bool|v = true <=> n = 1}
f70 = \x:int. eq x 1

f71 : {n:int|true} -> {v:bool|v = true <=> n = 1}
f71 = \x:int. eq 1 x
