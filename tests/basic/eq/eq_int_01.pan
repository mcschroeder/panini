eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f2 : {v:bool|v=false}
f2 = eq 1 1

f3 : {v:bool|false}
f3 = eq 1 1
