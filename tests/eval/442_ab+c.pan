import axioms

f442 : {s:string|?} -> unit
f442 = \s:string.
  let p = match s "c" in
  if p then
    unit
  else
    let c0 = charAt s 0 in
    let p0 = eqChar c0 'a' in
    let c1 = charAt s 1 in
    let p1 = eqChar c1 'b' in
    let q1 = and p0 p1 in
    let n = length s in
    let q2 = eq n 2 in
    let q = and q1 q2 in
    assert q
