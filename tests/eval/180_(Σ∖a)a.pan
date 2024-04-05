import axioms

f180 : {s:string|?} -> unit
f180 = \s:string.
  let c1 = charAt s 0 in
  let p1 = eqChar c1 'a' in
  let c2 = charAt s 1 in
  let p2 = eqChar c2 'a' in
  let p3 = not p1 in
  let p4 = and p3 p2 in
  let n = length s in
  let p5 = eq n 2 in
  let p6 = and p4 p5 in
  assert p6
