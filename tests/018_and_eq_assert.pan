and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}
eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }
assert : { b:𝔹 | b = true } → 𝟙

f0 : {z:unit|?}
f0 =
  let p1 = eq 1 1 in
  let p2 = eq 1 1 in
  let p3 = and p1 p2 in
  assert p3

f1 : {z:unit|?}
f1 =
  let p1 = eq 1 1 in
  let p2 = eq 2 1 in
  let p3 = and p1 p2 in
  assert p3  

f2 : {x:int|?} -> {z:unit|?}
f2 = \x:int.
  let p1 = eq x 1 in
  let p2 = eq x 1 in
  let p3 = and p1 p2 in
  assert p3

f3 : {x:int|x=1} -> {z:unit|?}
f3 = \x:int.
  let p1 = eq x 1 in
  let p2 = eq x 1 in
  let p3 = and p1 p2 in
  assert p3

f4 : {x:int|x=2} -> {z:unit|?}
f4 = \x:int.
  let p1 = eq x 1 in
  let p2 = eq x 1 in
  let p3 = and p1 p2 in
  assert p3

f5 : {x:int|?} -> {z:unit|true}
f5 = \x:int.
  let p1 = eq x 1 in
  let p2 = eq x 1 in
  let p3 = and p1 p2 in
  assert p3

f6 : {x:int|?} -> {z:unit|false}
f6 = \x:int.
  let p1 = eq x 1 in
  let p2 = eq x 1 in
  let p3 = and p1 p2 in
  assert p3

f7 : {x:int|?} -> {y:int|?} -> {z:unit|?}
f7 = \x:int. \y:int.
  let p1 = eq x 1 in
  let p2 = eq y 1 in
  let p3 = and p1 p2 in
  assert p3

f8 : {x:int|?} -> {y:int|?} -> {z:unit|true}
f8 = \x:int. \y:int.
  let p1 = eq x 1 in
  let p2 = eq y 1 in
  let p3 = and p1 p2 in
  assert p3

f9 : {x:int|x=1} -> {y:int|y=1} -> {z:unit|?}
f9 = \x:int. \y:int.
  let p1 = eq x 1 in
  let p2 = eq y 1 in
  let p3 = and p1 p2 in
  assert p3
