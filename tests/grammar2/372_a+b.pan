import axioms

f372 : {s:string|?} -> unit
f372 = \s:string.
  let c = charAt s 0 in
  let p1 = match c "a" in
  let p2 = match c "b" in
  let p3 = or p1 p2 in
  let n = length s in
  let p4 = eq n 1 in
  let p5 = and p3 p4 in
  assert p5
