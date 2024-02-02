eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }
assert : { b:𝔹 | b = true } → 𝟙

f110 = \x:int.
  let p = eq x 1 in
  assert p

f111 = \x:int.
  let p = eq 1 x in
  assert p

