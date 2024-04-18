import axioms

f093 : {s:string|?} -> unit
f093 = \s:string.
  let p1 = match s "a" in
  let n = length s in
  let p2 = eq n 0 in
  let p3 = or p1 p2 in
  assert p3
