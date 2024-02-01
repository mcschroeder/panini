eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }
assert : { b:𝔹 | b = true } → 𝟙

f41 : {x:int|x=2} -> unit
f41 = \x:int.
  let p = eq x 1 in
  assert p

f43 : {x:int|x>0} -> unit
f43 = \x:int.
  let p = eq x 1 in
  assert p


f51 : {x:int|x=2} -> unit
f51 = \x:int.
  let p = eq 1 x in
  assert p

f53 : {x:int|x>0} -> unit
f53 = \x:int.
  let p = eq 1 x in
  assert p


