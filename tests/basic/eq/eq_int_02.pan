eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f0 = eq 1 2

f1 : {v:bool|?}
f1 = eq 1 2

f2 : {v:bool|v=false}
f2 = eq 1 2
