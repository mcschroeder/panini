import axioms

f300 : {s:string|?} -> unit
f300 = \s:string.
  let c1 = charAt s 0 in
  let p1 = eqChar c1 'a' in
  let _ = assert p1 in
  let c2 = charAt s 1 in
  let p2 = eqChar c2 'b' in
  assert p2
