eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f30 : {v:bool|false}
f30 = eq 1 1

f31 : {v:bool|false}
f31 = eq 1 2

