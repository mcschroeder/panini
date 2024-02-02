eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f50 : {v:bool|v=false}
f50 = let x = 1 in eq x 1

f51 : {v:bool|v=false}
f51 = let x = 1 in eq 1 x

f52 : {v:bool|v=true}
f52 = let x = 1 in eq x 2

f53 : {v:bool|v=true}
f53 = let x = 2 in eq 1 x

