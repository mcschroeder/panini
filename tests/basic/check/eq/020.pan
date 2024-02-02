eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f20 : {v:bool|v=false}
f20 = eq 1 1

f21 : {v:bool|v=true}
f21 = eq 1 2
