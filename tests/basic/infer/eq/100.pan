eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }
assert : { b:𝔹 | b = true } → 𝟙

f100 : {v:unit|?}
f100 = 
  let p = eq 1 2 in 
  assert p

f101 : {v:unit|?}
f101 = 
  let x = 1 in
  let p = eq x 2 in 
  assert p

