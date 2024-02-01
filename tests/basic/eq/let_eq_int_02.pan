eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f00 = let x = 1 in eq x 2

f01 : {v:bool|?}
f01 = let x = 1 in eq x 2

f02 : {v:bool|v=false}
f02 = let x = 1 in eq x 2



f10 = let x = 2 in eq 1 x

f11 : {v:bool|?}
f11 = let x = 2 in eq 1 x

f12 : {v:bool|v=false}
f12 = let x = 2 in eq 1 x
