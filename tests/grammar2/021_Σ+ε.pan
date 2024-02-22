import axioms

f021 : {s:string|?} -> unit
f021 = \s:string.
  let n = length s in
  let p1 = eq n 0 in
  let p2 = eq n 1 in
  let p3 = or p1 p2 in
  assert p3
