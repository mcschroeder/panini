eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }
assert : { b:𝔹 | b = true } → 𝟙

f110 : unit
f110 = 
  let p = eq 1 1 in 
  assert p

f111 : unit
f111 = 
  let x = 1 in
  let p = eq x 1 in 
  assert p

