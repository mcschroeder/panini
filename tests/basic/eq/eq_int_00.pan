eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f0 = eq 1 1

f1 : {v:bool|?}
f1 = eq 1 1

f2 : {v:bool|v=true}
f2 = eq 1 1
