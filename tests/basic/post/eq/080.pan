eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }
assert : { b:𝔹 | b = true } → 𝟙

f80 : {v:unit|?}
f80 = 
  let p = eq 1 1 in 
  assert p

f81 : {v:unit|?}
f81 = 
  let x = 1 in
  let p = eq x 1 in 
  assert p

