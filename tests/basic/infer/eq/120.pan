eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }
assert : { b:𝔹 | b = true } → 𝟙

f120 : {x:int|?} -> {v:unit|?}
f120 = \x:int.
  let p = eq x 1 in
  assert p

f121 : {x:int|?} -> {v:unit|?}
f121 = \x:int.
  let p = eq 1 x in
  assert p

