eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }
assert : { b:𝔹 | b = true } → 𝟙

f40 = \x:int.
  let p = eq x 1 in
  assert p

f41 : {x:int|?} -> unit
f41 = \x:int.
  let p = eq x 1 in
  assert p

f42 : {x:int|x=1} -> unit
f42 = \x:int.
  let p = eq x 1 in
  assert p



f50 = \x:int.
  let p = eq 1 x in
  assert p

f51 : {x:int|?} -> unit
f51 = \x:int.
  let p = eq 1 x in
  assert p

f52 : {x:int|x=1} -> unit
f52 = \x:int.
  let p = eq 1 x in
  assert p
