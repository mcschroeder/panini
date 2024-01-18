eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }
assert : { b:𝔹 | b = true } → 𝟙

f0 : {v:unit|?}
f0 = 
  let p = eq 1 1 in 
  assert p

f1 : {v:unit|?}
f1 = 
  let p = eq 2 1 in 
  assert p

f2 : {v:unit|?}
f2 = 
  let x = 1 in
  let p = eq x 1 in
  assert p

f3 : {v:unit|?}
f3 = 
  let x = 2 in
  let p = eq x 1 in
  assert p

f4 : {n:int|true} -> {v:unit|?}
f4 = \x:int.
  let p = eq x 1 in
  assert p

f5 : {n:int|?} -> {v:unit|true}
f5 = \x:int.
  let p = eq x 1 in
  assert p

f6 : {n:int|?} -> {v:unit|false}
f6 = \x:int.
  let p = eq x 1 in
  assert p

f7 : {n:int|?} -> {v:unit|?}
f7 = \x:int.
  let p = eq x 1 in
  assert p

f8 : {n:int|n=1} -> {v:unit|?}
f8 = \x:int.
  let p = eq x 1 in
  assert p

f9 : {n:int|n=2} -> {v:unit|?}
f9 = \x:int.
  let p = eq x 1 in
  assert p
