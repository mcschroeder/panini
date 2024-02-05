eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f00 : {v:bool|?}
f00  = eq 1 1

f01 : {v:bool|?}
f01 = eq 1 2
