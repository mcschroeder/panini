eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }
assert : { b:𝔹 | b = true } → 𝟙

f130 : {x:int|x=1} -> unit
f130 = \x:int.
  let p = eq x 1 in
  assert p

f131 : {x:int|x=1} -> unit
f131 = \x:int.
  let p = eq 1 x in
  assert p

