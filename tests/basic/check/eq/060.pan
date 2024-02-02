eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f60 : {v:bool|false}
f60 = let x = 1 in eq x 1

f61 : {v:bool|false}
f61 = let x = 1 in eq 1 x

f62 : {v:bool|false}
f62 = let x = 1 in eq x 2

f63 : {v:bool|false}
f63 = let x = 2 in eq 1 x
