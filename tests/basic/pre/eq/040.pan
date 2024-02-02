eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }
assert : { b:𝔹 | b = true } → 𝟙

f40 : {x:int|?} -> unit
f40 = \x:int.
  let p = eq x 1 in
  assert p

f41 : {x:int|?} -> unit
f41 = \x:int.
  let p = eq 1 x in
  assert p
