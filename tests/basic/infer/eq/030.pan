eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f30 = let x = 1 in eq x 1

f31 = let x = 1 in eq 1 x

f32 = let x = 1 in eq x 2

f33 = let x = 2 in eq 1 x

