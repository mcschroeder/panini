import axioms

f370 : {s:string|?} -> unit
f370 = \s:string.
  let p1 = match s "a" in
  let p2 = match s "b" in
  let p3 = or p1 p2 in
  assert p3
