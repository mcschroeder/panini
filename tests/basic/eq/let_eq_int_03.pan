eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f02 : {v:bool|v=true}
f02 = let x = 1 in eq x 2

f04 : {v:bool|false}
f04 = let x = 1 in eq x 2


f12 : {v:bool|v=true}
f12 = let x = 2 in eq 1 x

f24 : {v:bool|false}
f24 = let x = 2 in eq 1 x