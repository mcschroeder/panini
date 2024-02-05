eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f60 : {n:int|?} -> {v:bool|?}
f60 = \x:int. eq x 1

f61 : {n:int|?} -> {v:bool|?}
f61 = \x:int. eq 1 x
