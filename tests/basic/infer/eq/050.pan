eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f50 = \x:int. eq x 1

f51 = \x:int. eq 1 x

