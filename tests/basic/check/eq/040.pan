eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f40 : {v:bool|v=true}
f40 = let x = 1 in eq x 1

f41 : {v:bool|v=true}
f41 = let x = 1 in eq 1 x

f42 : {v:bool|v=false}
f42 = let x = 1 in eq x 2

f43 : {v:bool|v=false}
f43 = let x = 2 in eq 1 x

