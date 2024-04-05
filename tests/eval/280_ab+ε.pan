import axioms

f280 : {s:string|?} -> unit
f280 = \s:string.
  let p1 = match s "ab" in
  let p2 = match s "" in
  let p3 = or p1 p2 in
  assert p3
