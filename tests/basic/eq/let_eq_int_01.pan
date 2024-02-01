eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f02 : {v:bool|v=false}
f02 = let x = 1 in eq x 1

f04 : {v:bool|false}
f04 = let x = 1 in eq x 1


f12 : {v:bool|v=false}
f12 = let x = 1 in eq 1 x

f14 : {v:bool|false}
f14 = let x = 1 in eq 1 x
