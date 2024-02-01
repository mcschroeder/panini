eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f2 : {v:bool|v=true}
f2 = eq 1 2

f4 : {v:bool|false}
f4 = eq 1 2
