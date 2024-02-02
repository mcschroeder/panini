eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }
assert : { b:𝔹 | b = true } → 𝟙

f140 : {x:int|x=2} -> unit
f140 = \x:int.
  let p = eq x 1 in
  assert p

f141 : {x:int|x>0} -> unit
f141 = \x:int.
  let p = eq x 1 in
  assert p

f142 : {x:int|x=2} -> unit
f142 = \x:int.
  let p = eq 1 x in
  assert p

f143 : {x:int|x>0} -> unit
f143 = \x:int.
  let p = eq 1 x in
  assert p

