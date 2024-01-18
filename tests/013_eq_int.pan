eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f0 = eq 1 1

f1 : bool
f1 = eq 1 1

f2 : {v:bool|?}
f2 = eq 1 1

f3 = eq 2 1

f4 : bool
f4 = eq 2 1

f5 : {v:bool|?}
f5 = eq 2 1

f6 = let x = 1 in eq x 1

f7 : bool
f7 = let x = 1 in eq x 1

f8 : {v:bool|?}
f8 = let x = 1 in eq x 1

f9 = let x = 2 in eq x 1

f10 : bool
f10 = let x = 2 in eq x 1

f11 : {v:bool|?}
f11 = let x = 2 in eq x 1
