eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f10 : {n:int|?} -> {v:bool|v=true}
f10 = \x:int. eq x 1

f11 : {n:int|?} -> {v:bool|v=true}
f11 = \x:int. eq 1 x
