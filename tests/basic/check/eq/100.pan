eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f100 : {n:int|n/=1} -> {v:bool|v=true}
f100 = \x:int. eq x 1

f101 : {n:int|n/=1} -> {v:bool|v=true}
f101 = \x:int. eq 1 x

f102 : {n:int|n=1} -> {v:bool|v=false}
f102 = \x:int. eq x 1

f103 : {n:int|n=1} -> {v:bool|v=false}
f103 = \x:int. eq 1 x

f104 : {n:int|n/=1} -> {v:bool|v=true}
f104 = \x:int. eq x 2

f105 : {n:int|n/=1} -> {v:bool|v=true}
f105 = \x:int. eq 2 x

f106 : {n:int|n=2} -> {v:bool|v=false}
f106 = \x:int. eq x 2

f107 : {n:int|n=2} -> {v:bool|v=false}
f107 = \x:int. eq 2 x

