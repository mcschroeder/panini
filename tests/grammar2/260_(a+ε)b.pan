import axioms

f260 : {s:string|?} -> unit
f260 = \s:string.
  let p1 = match s "ab" in
  let p2 = match s "b" in
  let p3 = or p1 p2 in
  assert p3
