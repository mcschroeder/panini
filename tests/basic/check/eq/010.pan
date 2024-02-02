eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f10 : {v:bool|v=true}
f10 = eq 1 1

f11 : {v:bool|v=false}
f11 = eq 1 2

