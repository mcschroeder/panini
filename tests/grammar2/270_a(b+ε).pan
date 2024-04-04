import axioms

f270 : {s:string|?} -> unit
f270 = \s:string.
  let p1 = match s "ab" in
  let p2 = match s "a" in
  let p3 = or p1 p2 in
  assert p3
