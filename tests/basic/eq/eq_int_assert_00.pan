eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }
assert : { b:𝔹 | b = true } → 𝟙

f00 = 
  let p = eq 1 1 in 
  assert p

f01 : unit
f01 = 
  let p = eq 1 1 in 
  assert p

f10 = 
  let x = 1 in
  let p = eq x 1 in 
  assert p

f11 : unit
f11 = 
  let x = 1 in
  let p = eq x 1 in 
  assert p
